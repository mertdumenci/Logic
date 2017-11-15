open Core
open Ir

module QID = QualifiedIdentity
module Env = Sawja_pack.Live_bir.Env

module Edge = struct
  type t = {
      formula: Ir.expr;
      rename: (Ir.var * Ir.var) list;
    }
  [@@deriving hash, compare]

  let default = { formula = Ir.LBool true; rename = [] }
  let equal = (=)
end

module Vertex = struct
  type t = {
      loc: QualifiedIdentity.t;
      live: Ir.var list;
      lock: Ir.lock list;
      kind: Ir.vkind;
    }
  [@@deriving hash, compare]

  let equal = (=)
end


include Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Vertex)(Edge)


let add_action
  v v' expr rename lock k vartable graph
  =
  let open Vertex in
  let open Edge in
  let get_var var = InstrGraph.java_to_var vartable v.InstrGraph.Instr.loc None var
    |> fst
  in
  let live_names env = env |> Env.elements |> List.map ~f:get_var in
  let start = {
    loc = v.InstrGraph.Instr.loc;
    live = live_names v.InstrGraph.Instr.live;
    lock = lock;
    kind = Ir.Instance;
  } in
  let finish = {
    loc = v'.InstrGraph.Instr.loc;
    live = live_names v'.InstrGraph.Instr.live;
    lock = lock;
    kind = k;
  } in
  let edge = {
    formula = expr;
    rename = rename;
  } in
  add_edge_e graph (E.create start edge finish)


let to_implication
      (instr_graph: InstrGraph.t)
      vartable
  =
  let build
        ((v: InstrGraph.V.t), (e: InstrGraph.E.label), (v': InstrGraph.V.t))
        ((graph: t), (lock: Ir.lock list)) =
    let open Vertex in
    let open Edge in
    let instr = v.InstrGraph.Instr.instr in
    let loc = v.InstrGraph.Instr.loc in
    let (expr, rename, k) = match (InstrGraph.instr_to_expr vartable loc instr, e) with
      | (Some (expr, r, k), InstrGraph.Branch.True) -> (expr, r, k)
      | (Some (expr, r, k), InstrGraph.Branch.Goto) -> (expr, r, k)
      | (Some (expr, r, k), InstrGraph.Branch.False) -> (Ir.ExprCons (Ir.Not, expr), r, k)
      | (None, _) -> (Ir.LBool true, [], Ir.Instance)
    in
    let push_lock (lock: Ir.lock list) =
      match lock with
        | ((Ir.Lock i) :: _) -> Ir.Lock (i + 1) :: lock
        | _ -> [Ir.Lock 0]
    in
    let pop_lock (lock: Ir.lock list) =
      match lock with
        | (_ :: xs) -> xs
        | _ -> failwith "Trying to unlock a nonexistent lock."
    in
    let (expr, rename, lock, k) = match expr with
      | Ir.ELock -> (Ir.LBool true, [], push_lock lock, Ir.Instance)
      | Ir.EUnlock -> (Ir.LBool true, [], pop_lock lock, Ir.Instance)
      | _ -> (expr, rename, lock, k)
    in
    (add_action v v' expr rename lock k vartable graph, lock)
  in
  InstrGraph.fold_edges_e build instr_graph (empty, []) |> fst


let serialize (graph: t) =
  let collect_vertices v l =
    let open Vertex in
    let lives = v.live
                |> List.map ~f:Ir.jsonsexp_var
                |> String.concat ~sep:","
    in
    let lock = v.lock
                |> List.map ~f:Ir.jsonsexp_lock
                |> String.concat ~sep:","
    in
    (match v.kind with
     | Ir.Instance -> Printf.sprintf "\"%s\":{\"type\":\"%s\",\"live\":[%s], \"lock\":[%s]}"
                        (QID.as_path v.loc)
                        (Ir.string_of_vkind v.kind)
                        lives
                        lock
     | Ir.Query ex -> Printf.sprintf "\"%s\":{\"type\":\"%s\",\"query\":%s,\"live\":[%s], \"lock\":[%s]}"
                        (QID.as_path v.loc)
                        (Ir.string_of_vkind v.kind)
                        (Ir.jsonsexp_expr ex)
                        lives
                        lock
    ) :: l
  in
  let vertices = fold_vertex collect_vertices graph [] in
  let vlist = String.concat vertices ~sep:"," |> Printf.sprintf "{%s}" in
  let rename_str r =
    List.map ~f:(fun (a, b) -> Printf.sprintf "[%s,%s]"
                    (Ir.jsonsexp_var a) (Ir.jsonsexp_var b)) r
    |> String.concat ~sep:","
    |> Printf.sprintf "[%s]"
  in
  let edge_str (v, e, v') l =
    let open Edge in
    let open Vertex in
    Printf.sprintf "{\"start\":\"%s\",\"end\":\"%s\",\"formula\":%s,\"rename\":%s}"
                   (QID.as_path v.loc)
                   (QID.as_path v'.loc)
                   (Ir.jsonsexp_expr e.formula)
                   (rename_str e.rename)
    :: l
  in
  let edges = fold_edges_e edge_str graph [] in
  let elist = String.concat ~sep:"," edges |> Printf.sprintf "[%s]" in
  Printf.sprintf "{\"edges\":%s,\"vertices\":%s}" elist vlist
