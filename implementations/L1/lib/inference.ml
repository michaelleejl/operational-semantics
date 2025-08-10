open Grammar
open Types
open Store 
open Utils

exception TypeMismatchException


type deriv = {
  sigma: sigma_t;
  e    : expr;
  t    : Types.t;
}

type deriv_loc = {
  sigma: sigma_t;
  l    : loc;
  t    : Types.t;
}

type deriv_tree = DerivAxiom of deriv
          | DerivInfer of deriv * (deriv_tree list)
          | DerivLoc of deriv_loc 

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
and infer_loc (Loc(l)) sigma = get_type l sigma

let infer e sigma = let t = infer_i e sigma in print_endline(judgement_to_str sigma e t)

let proj_type dt = match dt with
  | DerivLoc(d) -> d.t 
  | DerivAxiom(d) -> d.t 
  | DerivInfer(d, _) -> d.t 

let rec infer_deriv (e: expr) sigma = match e with
| Bool _ -> DerivAxiom({sigma = sigma; e = e; t = Bool})
| Int _ -> DerivAxiom({sigma = sigma; e = e; t = Int})
| Plus (e1, e2) -> 
  let d1 = infer_deriv e1 sigma in 
  let d2 = infer_deriv e2 sigma in 
  if ((proj_type d1) <> Int || (proj_type d2) <> Int) then raise TypeMismatchException else 
    DerivInfer({sigma = sigma; e = Plus(e1, e2); t = Int}, [d1; d2])
| Gteq (e1, e2) -> 
  let d1 = infer_deriv e1 sigma in 
  let d2 = infer_deriv e2 sigma in 
  if ((proj_type d1) <> Int || (proj_type d2) <> Int) then raise TypeMismatchException else 
    DerivInfer({sigma = sigma; e = Plus(e1, e2); t = Bool}, [d1; d2])
| If (e1, e2, e3) -> 
  let d1 = infer_deriv e1 sigma in 
  let d2 = infer_deriv e2 sigma in 
  let d3 = infer_deriv e2 sigma in 
  let typ = proj_type d2 in 
  if ((proj_type d1) <> Bool || (proj_type d3) <> typ) then raise TypeMismatchException else 
    DerivInfer({sigma = sigma; e = If(e1, e2, e3); t = typ}, [d1; d2])
| Assign (l, e) -> let d1 = infer_deriv_loc l sigma in 
                   let typ = proj_type d1 in 
                   let d2 = infer_deriv e sigma in 
                   (match typ with 
                   | Ref(t) -> 
                    if (proj_type d2 <> t) then raise TypeMismatchException
                    else DerivInfer({sigma = sigma; e = Assign(l, e); t = Unit}, [d1; d2])
                  | _ -> raise TypeMismatchException)
| Deref l -> let d = infer_deriv_loc l sigma in 
             let typ = proj_type d in 
             (match typ with 
             | Ref(t) -> DerivInfer({sigma = sigma; e = e; t = t}, [d])
             | _ -> raise TypeMismatchException)
| Skip -> DerivAxiom({sigma = sigma; e = e; t = Unit})
| Seq (e1, e2) -> 
  let d1 = infer_deriv e1 sigma in 
  let d2 = infer_deriv e2 sigma in 
  let typ = proj_type d2 in 
  if (proj_type d1 <> Unit) then raise TypeMismatchException else
  DerivInfer({sigma = sigma; e = e; t = typ}, [d1;d2])
| While (e1, e2) -> 
  let d1 = infer_deriv e1 sigma in 
  let d2 = infer_deriv e2 sigma in 
  let typ = proj_type d2 in 
  if (proj_type d1 <> Bool) then raise TypeMismatchException else
  DerivInfer({sigma = sigma; e = e; t = typ}, [d1;d2])
and infer_deriv_loc (Loc(l): loc) sigma = DerivLoc({sigma=sigma; l=Loc(l); t= get_type l sigma})
 
let deriv_to_str d = 
  let rec deriv_to_str_i (d: deriv_tree) n = match d with
  | DerivAxiom {sigma; e; t} -> n |- (sigma, e, t) 
  | DerivInfer ({sigma; e; t}, ds) -> List.fold_left (fun acc x -> acc ^ (deriv_to_str_i x (n+1))) (n |- (sigma, e, t)) (ds) 
  | DerivLoc {sigma; l; t} -> n |= (sigma, l, t)
in
deriv_to_str_i d 0

let verbose_infer e sigma = (infer_deriv e sigma) |> deriv_to_str |> print_endline

