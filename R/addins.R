runTable <- function() {
    context <- rstudioapi::getActiveDocumentContext()
    rstudioapi::sendToConsole(paste0("table(",context$selection[[1]]$text,")"), execute = TRUE)
}

runTableCC <- function() {
    context <- rstudioapi::getActiveDocumentContext()
    rstudioapi::sendToConsole(paste0("cc(table(",context$selection[[1]]$text,"))"), execute = TRUE)
}

runCC <- function() {
    context <- rstudioapi::getActiveDocumentContext()
    rstudioapi::sendToConsole(paste0("cc(",context$selection[[1]]$text,")"), execute = TRUE)
}
