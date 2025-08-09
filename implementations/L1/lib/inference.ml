open Grammar
open Types
open Store 
open Utils

exception TypeMismatchException

let rec infer_i (e: expr) (sigma: sigma_t) = match e with
| Bool _ -> Bool
| Int _ -> Int
| Skip -> Unit
| Plus (e1, e2) -> if (infer_i e1 sigma <> Int || infer_i e2 sigma <> Int) then raise TypeMismatchException else Int 
| Gteq (e1, e2) -> if (infer_i e1 sigma <> Int || infer_i e2 sigma <> Int) then raise TypeMismatchException else Bool 
| If (e1, e2, e3) -> if infer_i e1 sigma <> Bool then 
                      raise TypeMismatchException 
                     else 
                      let t2 = infer_i e2 sigma in
                      let t3 = infer_i e3 sigma in
                      if t2 <> t3 then raise TypeMismatchException else t2 
| Assign (l, e) -> (match infer_loc l sigma with
                    | Ref(t1) -> let t2 = infer_i e sigma in 
                    if t1 == t2 then Unit else raise TypeMismatchException
                    | _ -> raise TypeMismatchException)
| Deref l -> (match infer_loc l sigma with
                    | Ref(t1) -> t1
                    | _ -> raise TypeMismatchException)
| Seq (e1, e2) -> let t1 = infer_i e1 sigma in 
                  let t2 = infer_i e2 sigma in 
                  if (t1 <> Unit) then raise TypeMismatchException
                  else t2 
| While (e1, e2) -> let t1 = infer_i e1 sigma in 
                  let t2 = infer_i e2 sigma in 
                  if (t1 <> Bool || t2 <> Unit) then raise TypeMismatchException
                  else Unit 
and infer_loc (Loc(l)) sigma = Ref(get_type l sigma)

let infer e sigma = let t = infer_i e sigma in print_endline(judgement_to_str sigma e t)