open Implang;;

let rec verifyProg ast:stmt = 
  match ast with
  |  