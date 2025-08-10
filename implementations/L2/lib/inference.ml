open Grammar
open Types
open Store 
open Context 
open Utils

exception TypeMismatchException


type deriv = {
  sigma: sigma_t;
  gamma: gamma_t;
  e    : expr;
  t    : Types.t;
}

type deriv_var = {
  gamma: gamma_t;
  x    : param;
  t    : Types.t;
}

type deriv_loc = {
  sigma: sigma_t;
  l    : loc;
  t    : Types.t;
}

type deriv_tree = DerivAxiom of deriv
          | DerivVar of deriv_var 
          | DerivInfer of deriv * (deriv_tree list)
          | DerivLoc of deriv_loc 

let rec infer_i (e: expr) gamma (sigma: sigma_t) = match e with
| Bool _ -> Bool
| Int _ -> Int
| Skip -> Unit
| Plus (e1, e2) -> if (infer_i e1 gamma sigma <> Int || infer_i e2 gamma sigma <> Int) then raise TypeMismatchException else Int 
| Gteq (e1, e2) -> if (infer_i e1 gamma sigma <> Int || infer_i e2 gamma sigma <> Int) then raise TypeMismatchException else Bool 
| If (e1, e2, e3) -> if infer_i e1 gamma sigma <> Bool then 
                      raise TypeMismatchException 
                     else 
                      let t2 = infer_i e2 gamma sigma in
                      let t3 = infer_i e3 gamma sigma in
                      if t2 <> t3 then raise TypeMismatchException else t2 
| Assign (l, e) -> (match infer_loc l sigma with
                    | Ref(t1) -> let t2 = infer_i e gamma sigma in 
                    if t1 == t2 then Unit else raise TypeMismatchException
                    | _ -> raise TypeMismatchException)
| Deref l -> (match infer_loc l sigma with
                    | Ref(t1) -> t1
                    | _ -> raise TypeMismatchException)
| Seq (e1, e2) -> let t1 = infer_i e1 gamma sigma in 
                  let t2 = infer_i e2 gamma sigma in 
                  if (t1 <> Unit) then raise TypeMismatchException
                  else t2 
| While (e1, e2) -> let t1 = infer_i e1 gamma sigma in 
                  let t2 = infer_i e2 gamma sigma in 
                  if (t1 <> Bool || t2 <> Unit) then raise TypeMismatchException
                  else Unit 
| Fun(x, t1, e) -> let t2 = infer_i e (extend gamma x t1) sigma in 
                   Arrow (t1, t2)
| App(e1, e2) -> let t = infer_i e1 gamma sigma in 
                 (match t with 
                 | Arrow(t1, t2) -> let t3 = infer_i e2 gamma sigma in 
                                    if t1 == t3 then t2 
                                    else raise TypeMismatchException
                 | _ -> raise TypeMismatchException)
| Var(n)      -> lookup_type_in_context gamma n 
| Let(x, t, e1, e2) -> let t1 = infer_i e1 gamma sigma in 
                       if t <> t1 then raise TypeMismatchException
                       else let t2 = infer_i e2 (extend gamma x t) sigma in t2
| LetRec(x, t1, t2, (y, t, e1), e2) -> 
  if t <> t1 then raise TypeMismatchException
  else let t' = infer_i e1 (extend (extend gamma y t) x (Arrow (t1, t2))) sigma in 
  if t' <> t2 then raise TypeMismatchException
  else let t'' = infer_i e2 (extend gamma x (Arrow (t1, t2))) sigma in 
  t'' 
and infer_loc (Loc(l)) sigma = get_type l sigma

let infer e gamma sigma = let t = infer_i e gamma sigma in print_endline(judgement_to_str sigma gamma e t)

let proj_type dt = match dt with
  | DerivLoc(d) -> d.t 
  | DerivVar(d) -> d.t
  | DerivAxiom(d) -> d.t 
  | DerivInfer(d, _) -> d.t 

let rec infer_deriv (e: expr) gamma sigma = match e with
| Bool _ -> DerivAxiom({sigma = sigma; gamma = gamma; e = e; t = Bool})
| Int _ -> DerivAxiom({sigma = sigma; gamma = gamma; e = e; t = Int})
| Plus (e1, e2) -> 
  let d1 = infer_deriv e1 gamma sigma in 
  let d2 = infer_deriv e2 gamma sigma in 
  if ((proj_type d1) <> Int || (proj_type d2) <> Int) then raise TypeMismatchException else 
    DerivInfer({sigma = sigma; gamma = gamma; e = Plus(e1, e2); t = Int}, [d1; d2])
| Gteq (e1, e2) -> 
  let d1 = infer_deriv e1 gamma sigma in 
  let d2 = infer_deriv e2 gamma sigma in 
  if ((proj_type d1) <> Int || (proj_type d2) <> Int) then raise TypeMismatchException else 
    DerivInfer({sigma = sigma; gamma = gamma; e = Plus(e1, e2); t = Bool}, [d1; d2])
| If (e1, e2, e3) -> 
  let d1 = infer_deriv e1 gamma sigma in 
  let d2 = infer_deriv e2 gamma sigma in 
  let d3 = infer_deriv e2 gamma sigma in 
  let typ = proj_type d2 in 
  if ((proj_type d1) <> Bool || (proj_type d3) <> typ) then raise TypeMismatchException else 
    DerivInfer({sigma = sigma; gamma = gamma; e = If(e1, e2, e3); t = typ}, [d1; d2])
| Assign (l, e) -> let d1 = infer_deriv_loc l sigma in 
                   let typ = proj_type d1 in 
                   let d2 = infer_deriv e gamma sigma in 
                   (match typ with 
                   | Ref(t) -> 
                    if (proj_type d2 <> t) then raise TypeMismatchException
                    else DerivInfer({sigma = sigma; gamma = gamma; e = Assign(l, e); t = Unit}, [d1; d2])
                  | _ -> raise TypeMismatchException)
| Deref l -> let d = infer_deriv_loc l sigma in 
             let typ = proj_type d in 
             (match typ with 
             | Ref(t) -> DerivInfer({sigma = sigma; gamma = gamma; e = e; t = t}, [d])
             | _ -> raise TypeMismatchException)
| Skip -> DerivAxiom({sigma = sigma; gamma = gamma; e = e; t = Unit})
| Seq (e1, e2) -> 
  let d1 = infer_deriv e1 gamma sigma in 
  let d2 = infer_deriv e2 gamma sigma in 
  let typ = proj_type d2 in 
  if (proj_type d1 <> Unit) then raise TypeMismatchException else
  DerivInfer({sigma = sigma; gamma = gamma; e = e; t = typ}, [d1;d2])
| While (e1, e2) -> 
  let d1 = infer_deriv e1 gamma sigma in 
  let d2 = infer_deriv e2 gamma sigma in 
  let typ = proj_type d2 in 
  if (proj_type d1 <> Bool) then raise TypeMismatchException else
  DerivInfer({sigma = sigma; gamma = gamma; e = e; t = typ}, [d1;d2])
| Fun(x, t, e) -> 
  let d = infer_deriv e (extend gamma x t) sigma in 
  let t2 = proj_type d in 
  DerivInfer({sigma = sigma; gamma = gamma; e = e; t =(Arrow (t, t2))}, [d])
| App(e1, e2) -> 
  let d1 = infer_deriv e1 gamma sigma in 
  let d2 = infer_deriv e2 gamma sigma in 
  let t = proj_type d1 in 
  let t' = proj_type d2 in 
  (match t with
  | Arrow(t1, t2) -> if t' <> t1 then raise TypeMismatchException else 
                     DerivInfer({sigma = sigma; gamma = gamma; e = App(e1, e2); t=t2}, [d1; d2])
  | _ -> raise TypeMismatchException)
| Let(x, t, e1, e2) -> let d1 = infer_deriv e1 gamma sigma in 
                       let t1 = proj_type d1 in 
                       if t <> t1 then raise TypeMismatchException
                       else let d2 = infer_deriv e2 (extend gamma x t) sigma in 
                       let t2 = proj_type d2 in
                       DerivInfer({sigma = sigma; gamma = gamma; e = Let(x, t, e1, e2); t=t2}, [d1; d2])
| LetRec(x, t1, t2, (y, t, e1), e2) -> 
  if t <> t1 then raise TypeMismatchException
  else let d1 = infer_deriv e1 (extend (extend gamma y t) x (Arrow (t1, t2))) sigma in 
  let t' = proj_type d1 in 
  if t' <> t2 then raise TypeMismatchException
  else let d2 = infer_deriv e2 (extend gamma x (Arrow (t1, t2))) sigma in 
  let t'' = proj_type d2 in 
  DerivInfer({sigma = sigma; gamma = gamma; e = LetRec(x, t1, t2, (y, t, e1), e2); t=t''}, [d1; d2])
| Var(n) -> let d = infer_deriv_var n gamma  in
            let t = proj_type d in
            DerivInfer({sigma = sigma; gamma = gamma; e = Var(n); t=t}, [d])
and infer_deriv_var n gamma = DerivVar({gamma=gamma; x=(lookup_name_in_context gamma n); t=(lookup_type_in_context gamma n)})
and infer_deriv_loc (Loc(l): loc) sigma = DerivLoc({sigma=sigma; l=Loc(l); t= get_type l sigma})
 
let deriv_to_str d = 
  let rec deriv_to_str_i (d: deriv_tree) n = match d with
  | DerivAxiom {sigma; gamma; e; t} -> n |- (sigma, gamma, e, t) 
  | DerivInfer ({sigma; gamma ; e; t}, ds) -> List.fold_left (fun acc x -> acc ^ (deriv_to_str_i x (n+1))) (n |- (sigma, gamma, e, t)) (ds) 
  | DerivLoc {sigma; l; t} -> n |= (sigma, l, t)
  | DerivVar {gamma; x ;t} -> n |+ (gamma, x, t)
in
deriv_to_str_i d 0

let verbose_infer e gamma sigma = (infer_deriv e gamma sigma) |> deriv_to_str |> print_endline