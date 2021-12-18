library(plumber)

#list(digits = 12)

source("helpers.R")

#' @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}
#' @post /tickersuche
#' @get /tickersuche
ticker_info_suchen_api = function(suche) {
  ticker_info_suchen(suche)}

#' @post /calc_f
#' @get /calc_f
calc_f_api = function(data, len1) {
  calc_f(data,len1)}

#' @serializer json list(digits = 12)
#' @post /getstocks
#' @get /getstocks
getstocks_api <- function(symbol,hist.len){
  getstocks(symbol,hist.len)}

#' @serializer json list(digits = 12)
#' @post /optimize_stocks_p2
#' @get /optimize_stocks_p2
optimize_stocks_p2_api<- function(req, npo) {
  optimize_stocks_p2(req, npo)}

#' @post /eff_front_plot_dat
#' @get /eff_front_plot_dat
eff_front_plot_dat_api<- function(Weights_F, tickers_F) {
  eff_front_plot_dat(Weights_F, tickers_F)}

#' @post /user_portf
#' @get /user_portf
user_portf_api <- function(portfo){
  user_portf(portfo)}

#' @serializer json list(digits = 12)
#' @post /log_rendite
#' @get /log_rendite
log_rendite_api<-function(req){
  log_rendite(req)
}

#' @serializer json list(digits = 12)
#' @post /fx
#' @get /fx
fx_api<-function(curr, hist.len){
  fx_fx(curr, hist.len)
}

#' @serializer json list(digits = 12)
#' @post /all_in_one
#' @get /all_in_one
all_in_one_api<-function(symbol,hist.len,curr){
  all_in_one(symbol,hist.len,curr)
}

#' @serializer htmlwidget
#' @post /efffront
#' @get /efffront
plotly_efffront_api<-function(symbol,hist.len,curr){
  plotly_efffront(symbol,hist.len,curr)
}