type point = {
  x: Z.t;
  y: Z.t;
} 

type ec_point = point option

type elliptic_curve = {
  p: Z.t;
  a: Z.t;
  b: Z.t;
  g: ec_point;
  n: Z.t;   (* n is prime and the order of G *)
  h: Z.t;   (* the number of points on the curve *)
}

type ecc_key = {
  d: Z.t;   (* random integer < n; this is the private key *)
  q: ec_point; (* q = d * G .. this is the public key *)
}

let add_points (p1: ec_point) (p2: ec_point) (a: Z.t) (p: Z.t)  : ec_point =
  let helper p1 p2 =
    let lambda = Z.mul (Z.sub p2.y p1.y) (Z.invert (Z.sub p2.x p1.x) p) in
    let x3 = Z.erem (Z.sub (Z.sub (Z.mul lambda lambda) p1.x) p2.x) p in
    let y3 = Z.erem (Z.sub (Z.mul (Z.sub p1.x x3) lambda) p1.y) p in
    { x = x3; y = y3 }
  in
  let double_help p1 =
    let lambda = Z.erem (Z.mul (Z.add (Z.mul (Z.of_int 3) (Z.mul p1.x p1.x)) a) (Z.invert (Z.mul (Z.of_int 2) p1.y) p)) p in
    let x = Z.erem (Z.sub (Z.mul lambda lambda) (Z.mul (Z.of_int 2) p1.x)) p in
    let y = Z.erem (Z.sub (Z.mul lambda (Z.sub p1.x x)) p1.y) p in
    { x = x; y = y }
  in
  match p1 with
  | Some p1_p -> begin
      match p2 with
        | Some p2_p -> 
            if (Z.equal p1_p.x p2_p.x) then begin
              if (Z.equal p1_p.y p2_p.y) then Some (double_help p1_p) else None
            end else Some (helper p1_p p2_p)
        | None -> p1
  end
  | None -> p2

let print_point (q: ec_point) =
  match q with
  | None -> print_string "(0, 0)"
  | Some q_ -> print_string "("; Z.print q_.x; print_string ","; Z.print q_.y; print_string ")\n"

(* implementation does not use bit-level operations *)
let multiply_point (p1: ec_point) (k_: Z.t) (a: Z.t) (p: Z.t) : ec_point =
  let k = ref Z.zero in
  k := k_ ;
  let n = ref None in
  n := p1;
  let q = ref None in
  while (not (Z.equal !k Z.zero)) do
    if (not (Z.equal (Z.logand !k Z.one) Z.zero)) then q := add_points !q !n a p;
    n := add_points !n !n a p;
    k := Z.shift_right !k 1
  done;
  !q
