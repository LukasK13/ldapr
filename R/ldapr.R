#' Search LDAP
#'
#' This function serves as interface to the console function ldapsearch.
#' It allows to bind to a LDAP server and search for attributes.
#' The syntax of this function is based on the syntax of ldapsearch
#' \link{http://www.openldap.org/software/man.cgi?query=ldapsearch&sektion=1&manpath=OpenLDAP+2.4-Release}
#'
#' @param ... Parameters for the call to ldapsearch. Each parameter must be named
#' (e.g. h for the host definition) and may also be NULL for an parameter
#' without value. Some values, like D must be double quoted for evaluation within
#' quotes.
#' @return Named list of returned attributes or NULL if the call wasn't successful.
#'
#' @details This package requires the library openldap to be installed (https://www.openldap.org/).
#'
#' @examples ldapsearch(x = NULL, h = 'ldap.myhost.org', D = '"QueryUserName\@myDomain"', w = 'QueryUserPassword', b = '"CN=SomeUser,OU=Users,DC=SomeDomain"')
#'
#' @export
ldapsearch <- function(...) {
  result = suppressWarnings(system(paste0('ldapsearch ', paste0(ifelse(lapply(names(c(...)), is.empty), "", "-"), names(c(...)),
                                                                ifelse(lapply(c(...), is.null), "", paste0(" ", c(...))), collapse = " "), " -LLL"),
                                   intern = T, wait = T))
  if (length(result) == 0) return(NULL)
  result = result[result != ""]
  for (i in length(result):1) {
    if (length(grep("^ .{+}$", result[i]))) {
      result[i - 1] = paste0(result[i-1], gsub("^ ", "", result[i]))
      result = result[-i]
    }
  }
  Reduce(c, lapply(result, function(attribute) {
    name = gsub(":", "", regmatches(attribute, regexpr("^[[:alnum:]]{+}\\:", attribute)))
    value = gsub("\\\\", "", gsub(paste0("^", name, "\\:{1,2} "), "", attribute))
    item = list(value)
    names(item) = name
    return(item)
  }))
}

#' Request LDAP ID
#'
#' This function serves as interface to the console function ldapwhoami
#' It allows to bind to a LDAP server and receive the user id.
#' The syntax of this function is based on the syntax of ldapwhoami
#' \link{http://www.openldap.org/software/man.cgi?query=ldapwhoami&sektion=1&manpath=OpenLDAP+2.4-Release}
#'
#' @param ... Parameters for the call to ldapwhoami Each parameter must be named
#' (e.g. h for the host definition) and may also be NULL for an parameter
#' without value. Some values, like D must be double quoted for evaluation within
#' quotes.
#' @return User ID or NULL if the call wasn't successful.
#'
#' @details This package requires the library openldap to be installed (https://www.openldap.org/).
#'
#' @examples ldapwhoami(x = NULL, h = 'ldap.myhost.org', D = '"QueryUserName\@myDomain"', w = 'QueryUserPassword')
#'
#' @export
ldapwhoami <- function(...) {
  result = suppressWarnings(system(paste0('ldapwhoami ', paste0("-", names(c(...)), ifelse(lapply(c(...), is.null), "", paste0(" ", c(...))), collapse = " ")),
                                   intern = T, wait = T))
  return(if(length(result)) gsub("u\\:", "", result) else NULL)
}
