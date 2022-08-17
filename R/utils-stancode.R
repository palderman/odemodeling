# Generate 'Stan' code for prior model (only parameters, no ODE solving)
generate_stancode_prior <- function(odefun_vars, loglik_vars, other_vars,
                                    compile) {
  all_vars <- c(odefun_vars, loglik_vars)
  params <- all_vars[sapply(all_vars, is_param)]
  tparams <- all_vars[sapply(all_vars, is_tparam)]
  all_vars <- c(params, tparams)
  all_decls <- lapply(all_vars, get_decl)
  dims <- dims_of_decls(all_decls)

  data_b <- generate_data_block(all_decls, list()) # just dimensions
  pars_b <- generate_params_block(params)
  tpars_b <- generate_transform_block("transformed parameters", tparams, TRUE)
  model_b <- generate_model_block(params, prior_mode = TRUE)
  code <- paste(data_b, pars_b, tpars_b, model_b, sep = "\n")
  code <- autoformat_stancode(code)

  # Return
  StanModelWithCode$new(code, dims, NULL, NULL, params, tparams, NULL, compile)
}

# Create the functions block
generate_functions_block <- function(odefun_add_sign,
                                     odefun_add_args,
                                     odefun_body,
                                     loglik_add_sign,
                                     loglik_body,
                                     before_ode_funs = "",
                                     after_ode_funs = "",
                                     allow_undefined = FALSE) {
  odefun <- generate_odefun(odefun_add_sign, odefun_body)
  if (nchar(loglik_body) > 0) {
    loglik <- generate_loglik(loglik_add_sign, loglik_body)
  } else {
    loglik <- ""
  }
  solvers <- functions_template()
  odefun_add_sign <- add_leading_comma(odefun_add_sign)
  odefun_add_args <- add_leading_comma(odefun_add_args)
  solvers <- fill_stancode_part(solvers, odefun_add_sign, "__ODEFUN_SIGN__")
  solvers <- fill_stancode_part(solvers, odefun_add_args, "__ODEFUN_ARGS__")
  code <- generate_block("functions", c(before_ode_funs, odefun, solvers,
                                        after_ode_funs, loglik))
  autoformat_stancode(code, allow_undefined)
}

# Create additional signature for function
generate_add_signature <- function(all_vars, argmode) {
  if (length(all_vars) == 0) {
    return("")
  }
  no_add_signature <- function(x) {
    name <- get_name(x)
    is_noadd <- name %in% c("t", "t0", "y0")
    return(!is_noadd)
  }
  all_vars <- all_vars[sapply(all_vars, no_add_signature)]
  all_vars <- unique(all_vars)

  data <- all_vars[sapply(all_vars, is_data)]
  tdata <- all_vars[sapply(all_vars, is_tdata)]
  params <- all_vars[sapply(all_vars, is_param)]
  tparams <- all_vars[sapply(all_vars, is_tparam)]

  data_vars <- c(data, tdata)
  par_vars <- c(params, tparams)
  data_decls <- lapply(data_vars, get_decl)
  par_decls <- lapply(par_vars, get_decl)
  dim_sign <- generate_dim_signatures(c(data_decls, par_decls), argmode)
  data_sign <- generate_var_signatures(data_decls, TRUE, argmode)
  par_sign <- generate_var_signatures(par_decls, FALSE, argmode)

  signature <- ""
  signature <- append_to_signature(signature, dim_sign)
  signature <- append_to_signature(signature, data_sign)
  signature <- append_to_signature(signature, par_sign)
  trimws(signature)
}

# Generate Stan code for the ODE function
generate_odefun <- function(add_signature, odefun_body) {
  signature <- "real t, vector y"
  signature <- append_to_signature(signature, add_signature)
  comment <- "\n// ODE system right-hand side \n"
  code <- paste0(comment, "vector odefun(", signature, ")")
  if (nchar(odefun_body) == 0) {
    warning("ODE function body is empty!")
  }
  paste0(code, "{\n", odefun_body, "\n}\n")
}

# Generate Stan code for the log likelihood function
generate_loglik <- function(add_signature, loglik_body) {
  signature <- "array[] vector y_sol"
  signature <- append_to_signature(signature, add_signature)
  comment <- "\n// Compute log likelihood given ODE solution y_sol\n"
  code <- paste0(comment, "real log_likelihood(", signature, ")")
  if (nchar(loglik_body) == 0) {
    warning("log likelihood function body is empty!")
  }
  paste0(code, "{\n", loglik_body, "\n}\n")
}

# Create the data block
generate_data_block <- function(all_decls, data) {
  dims_code <- generate_dim_declarations(all_decls)
  dvars_code <- generate_var_declarations(data)
  code <- generate_block("data", c(dims_code, dvars_code))
  autoformat_stancode(code)
}

# Create a transform block (transformed data, transformed params, or gq)
generate_transform_block <- function(name, transforms, reorder) {
  is_ysol_tpar <- function(x) x$decl$name == "y_sol_tpar"
  is_loglik_tpar <- function(x) x$decl$name == "log_lik_tpar"
  if (reorder) {
    i1 <- which(sapply(transforms, is_ysol_tpar) == TRUE)
    i2 <- which(sapply(transforms, is_loglik_tpar) == TRUE)
    L <- length(transforms)
    i_first <- setdiff(seq_len(L), c(i1, i2))
    transforms <- c(transforms[i_first], transforms[i1], transforms[i2])
  }

  decls <- lapply(transforms, get_decl)
  codes <- paste(lapply(transforms, get_code), collapse = "\n")
  decls <- generate_var_declarations(decls)
  generate_block(name, c(decls, codes))
}

# Create the parameters block
generate_params_block <- function(params) {
  decls <- lapply(params, get_decl)
  dvars_code <- generate_var_declarations(decls)
  generate_block("parameters", c(dvars_code))
}

# Create the parameters block
generate_model_block <- function(params, prior_mode) {
  codes <- paste(lapply(params, get_prior_code), collapse = "\n")
  if (prior_mode) {
    target <- ""
  } else {
    target <- "target += log_lik_tpar;"
  }
  generate_block("model", c(codes, target))
}

# Create a block of Stan code
generate_block <- function(name, parts) {
  body <- ""
  for (p in parts) {
    if (nchar(p) > 0) {
      body <- paste0(body, "\n", p)
    }
  }
  if (nchar(body) == 0) {
    return("")
  }
  paste0(name, " {\n", body, "\n}\n")
}

# Dimensions signatures
generate_dim_signatures <- function(vars, argmode) {
  checkmate::assert_list(vars, "StanDeclaration")
  dim_vars <- list()
  for (var in vars) {
    dim_vars <- c(dim_vars, var$get_dims())
  }
  generate_var_signatures(dim_vars, TRUE, argmode)
}

# Variables declaration code
generate_var_signatures <- function(vars, data, argmode) {
  if (length(vars) == 0) {
    return("")
  }
  vars <- unique(vars)
  checkmate::assert_list(vars, "StanDeclaration")
  if (argmode) {
    getter <- function(x) x$name
  } else {
    getter <- function(x) x$signature()
  }
  if (data && !argmode) {
    pre <- "data"
  } else {
    pre <- ""
  }
  signs <- unique(sapply(vars, getter))
  code <- paste(pre, signs, collapse = ", ")
  trimws(code)
}

# Get dimensions of each declaration
dims_of_decls <- function(decls) {
  checkmate::assert_list(decls, "StanDeclaration")
  dim_decls <- list()
  for (var in decls) {
    dim_decls <- c(dim_decls, var$get_dims())
  }
  unique(dim_decls)
}

# Dimensions declaration code
generate_dim_declarations <- function(vars) {
  dim_vars <- dims_of_decls(vars)
  generate_var_declarations(dim_vars)
}

# Variables declaration code
generate_var_declarations <- function(vars) {
  checkmate::assert_list(vars, "StanDeclaration")
  vars <- unique(vars)
  get_decl <- function(x) {
    x$declaration()
  }
  lines <- sapply(vars, get_decl)
  lines <- unique(lines)
  if (length(lines) == 0) {
    return("")
  }
  paste(lines, ";", sep = "", collapse = "\n")
}

# Template 'Stan' model code
functions_template <- function() {
  filepath <- system.file("template_functions.stan", package = "odemodeling")
  read_file_lines(filepath)
}

# Helper function
fill_stancode_part <- function(code, replacement, placeholder) {
  if (is.null(replacement)) {
    replacement <- paste0(placeholder, " <<<<<<<<<<<< MISSING !")
  }
  gsub(pattern = placeholder, fixed = TRUE, x = code, replacement = replacement)
}

# Generate a call to the ODE solver
generate_solver_call <- function(odefun_vars,
                                 num_steps_arg = "num_steps",
                                 y0_arg = "y0", t0_arg = "t0",
                                 t_arg = "t", Nt_arg = "Nt"){

  odefun_add_args <- generate_add_signature(odefun_vars, TRUE)

  so_args <- paste("solver, rel_tol, abs_tol, max_num_steps",
                   num_steps_arg,
                   y0_arg,
                   t0_arg,
                   t_arg,
                   sep = ", ")

  solve_ode_args <- append_to_signature(so_args, odefun_add_args)

  code <- paste0("solve_ode(", solve_ode_args, ")")

  return(code)
}

new_hpp_name <- function(stan_file){

  # Create new hpp file name
  return(gsub("\\.stan", "_functions.hpp", stan_file))

}

abs_path <- function(file_name){
  if(substr(file_name, 1, 1) != "/"){
    # stan_file is relative path
    # convert to absolute path:
    file_name <- file.path(getwd(), file_name)
  }
  return(file_name)
}

# Define function for setting model namespace in custom header
write_custom_hpp <- function(stan_file, hpp_code){

  # Convert relative path to absolute path:
  stan_file <- abs_path(stan_file)

  # Extract model name
  model_name <- gsub("\\.stan$", "",
                     basename(stan_file)
                     )

  # Create new hpp file name
  hpp_file <- new_hpp_name(stan_file)

  # Replace current model namespace with model name from stan file
  #   and write to new hpp file

  write(c("#include <boost/math/tools/promotion.hpp>",
          "#include <stan/math/rev/core.hpp>",
          "#include <ostream>",
          paste0("namespace ", model_name, "_model_namespace{"),
          hpp_code,
          "}"),
    hpp_file)

  # Create cpp_options to be passed to cmdstan_model()
  cpp_options <- list(USER_HEADER = hpp_file)

  return(invisible(cpp_options))
}

up_to_date <- function(target, dependency){

  # Get info on target file
  t_info <- file.info(target)

  # Get info on dependency
  d_info <- file.info(dependency)

  # Return TRUE if target modified time is after dependency modified time
  return(d_info$mtime < t_info$mtime)

}

#' @export
#'
compile_cmdstan_model <- function(stan_code, hpp_code = "",
                                  dir = NULL, model_name = NULL,
                                  stanc_options = list("allow-undefined")){

  if(is.null(dir)){
    dir <- getOption("cmdstanr_write_stan_file_dir",
                     tempdir())
  }

  if(is.null(model_name)){
    stan_file_basename <- NULL
  }else{
    stan_file_basename <- paste0(model_name, ".stan")
  }

  stan_file <- cmdstanr::write_stan_file(code = stan_code,
                                         dir = dir,
                                         basename = stan_file_basename)

  if(is.null(model_name)) model_name <- gsub("\\.stan$", "",
                                             basename(stan_file))

  exec_path <- gsub("\\.stan$", "", stan_file)

  # Create new hpp file name
  hpp_name <- new_hpp_name(stan_file)

  stan_dir <- dirname(stan_file)

  # Check target hpp file and update, if needed
  cpp_options <- write_custom_hpp(stan_file, hpp_code)

  if(file.exists(exec_path) &&
     !up_to_date(exec_path, hpp_name)){
    file.remove(exec_path)
  }

  include_paths <- dirname(cpp_options[["USER_HEADER"]])

  model <- cmdstanr::cmdstan_model(stan_file = stan_file,
                                   include_paths = include_paths,
                                   cpp_options = cpp_options,
                                   stanc_options = stanc_options)

  return(model)
}

