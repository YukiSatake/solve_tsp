type t = (int * float * float) list

let image_of_tsp (nodes: t) =
  let open Gg in
  let open Vg in
  let aspect = 1.618 in
  let size = Size2.v (aspect *. 100.) 100. in
  let view = Box2.v P2.o (Size2.v aspect 1.) in
  let image = I.const (Color.v_srgb 0.314 0.784 0.471) in
  size, view, image

let render size view image =
  let open Gg in
  let open Vg in
  let res = 300. /. 0.0254 in
  let fmt = `Png (Size2.v res res) in
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let r = Vgr.create ~warn (Vgr_cairo.stored_target fmt) (`Channel stdout) in
  ignore @@ Vgr.render r (`Image (size, view, image));
  ignore @@ Vgr.render r `End
