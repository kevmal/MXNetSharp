// F# operators have issues with inheritance. For example exp uses (^T: (static member Exp : ^T -> ^T) (x)) but
// if the case of the Symbol api as it is now calling exp on any subtype ^T :> Symbol will expect the output to
// also be ^T which generally makes no sense. Each exp call would need an upcast, `exp (input :> Symbol)` which
// gets ugly. The following module can be opened for "elegant" expressions such as `(exp 5.0) * (exp symbol)`
namespace MXNetSharp

open System

#nowarn "77"

module InternalPrimitiveOperatorHelpers =     
    type ExpOp = ExpOp with
        static member inline Exp(ExpOp, x : ^T) : ^T = exp x
        static member inline Exp(ExpOp, x : ^T) : ^Y = (^T: (member ApplyExp : unit -> ^Y) (x))
    let inline internal expHelper (t : ^op) (x : ^t) : ^y = ((^op or ^t) : (static member Exp : ^op * ^t -> ^y)(t,x))

    type LogOp = LogOp with
        static member inline Log(LogOp, x : ^T) : ^T = log x
        static member inline Log(LogOp, x : ^T) : ^Y = (^T: (member ApplyLog : unit -> ^Y) (x))
    let inline internal logHelper (t : ^op) (x : ^t) : ^y = ((^op or ^t) : (static member Log : ^op * ^t -> ^y)(t,x))
        
    type AbsOp = AbsOp with
        static member inline Abs(AbsOp, x : ^T) : ^T = abs x
        static member inline Abs(AbsOp, x : ^T) : ^Y = (^T: (member ApplyAbs : unit -> ^Y) (x))
    let inline internal absHelper (t : ^op) (x : ^t) : ^y = ((^op or ^t) : (static member Abs : ^op * ^t -> ^y)(t,x))

    type AcosOp = AcosOp with
        static member inline Acos(AcosOp, x : ^T) : ^T = acos x
        static member inline Acos(AcosOp, x : ^T) : ^Y = (^T: (member ApplyAcos : unit -> ^Y) (x))
    let inline internal acosHelper (t : ^op) (x : ^t) : ^y = ((^op or ^t) : (static member Acos : ^op * ^t -> ^y)(t,x))

    type AsinOp = AsinOp with
        static member inline Asin(AsinOp, x : ^T) : ^T = asin x
        static member inline Asin(AsinOp, x : ^T) : ^Y = (^T: (member ApplyAsin : unit -> ^Y) (x))
    let inline internal asinHelper (t : ^op) (x : ^t) : ^y = ((^op or ^t) : (static member Asin : ^op * ^t -> ^y)(t,x))

    type AtanOp = AtanOp with
        static member inline Atan(AtanOp, x : ^T) : ^T = atan x
        static member inline Atan(AtanOp, x : ^T) : ^Y = (^T: (member ApplyAtan : unit -> ^Y) (x))
    let inline internal atanHelper (t : ^op) (x : ^t) : ^y = ((^op or ^t) : (static member Atan : ^op * ^t -> ^y)(t,x))

    //TODO: This doesn't work
    type AtanOp2 = AtanOp2 with
        static member inline Atan2(AtanOp2, x : ^T, y : ^T) : ^T = atan2 x y
        static member inline Atan2(AtanOp2, x : ^T, y : ^T2) : ^Y = ((^T or ^T2): (static member ArcTan2 : ^T * ^T2 -> ^Y) (x, y))
    let inline internal atan2Helper (t : ^op) (x : ^t) (y : ^t2) : ^y = ((^op or ^t or ^t2) : (static member Atan2 : ^op * ^t * ^t2-> ^y)(t, x, y))

    type CeilingOp = CeilingOp with
        static member inline Ceiling(CeilingOp, x : ^T) : ^T = ceil x
        static member inline Ceiling(CeilingOp, x : ^T) : ^Y = (^T: (member ApplyCeiling : unit -> ^Y) (x))
    let inline internal ceilingHelper (t : ^op) (x : ^t) : ^y = ((^op or ^t) : (static member Ceiling : ^op * ^t -> ^y)(t,x))

    type FloorOp = FloorOp with
        static member inline Floor(FloorOp, x : ^T) : ^T = floor x
        static member inline Floor(FloorOp, x : ^T) : ^Y = (^T: (member ApplyFloor : unit -> ^Y) (x))
    let inline internal floorHelper (t : ^op) (x : ^t) : ^y = ((^op or ^t) : (static member Floor : ^op * ^t -> ^y)(t,x))

    type TruncateOp = TruncateOp with
        static member inline Truncate(TruncateOp, x : ^T) : ^T = truncate x
        static member inline Truncate(TruncateOp, x : ^T) : ^Y = (^T: (member ApplyTruncate : unit -> ^Y) (x))
    let inline internal truncateHelper (t : ^op) (x : ^t) : ^y = ((^op or ^t) : (static member Truncate : ^op * ^t -> ^y)(t,x))

    type RoundOp = RoundOp with
        static member inline Round(RoundOp, x : ^T) : ^T = round x
        static member inline Round(RoundOp, x : ^T) : ^Y = (^T: (member ApplyRound : unit -> ^Y) (x))
    let inline internal roundHelper (t : ^op) (x : ^t) : ^y = ((^op or ^t) : (static member Round : ^op * ^t -> ^y)(t,x))

    type Log10Op = Log10Op with
        static member inline Log10(Log10Op, x : ^T) : ^T = log10 x
        static member inline Log10(Log10Op, x : ^T) : ^Y = (^T: (member ApplyLog10 : unit -> ^Y) (x))
    let inline internal log10Helper (t : ^op) (x : ^t) : ^y = ((^op or ^t) : (static member Log10 : ^op * ^t -> ^y)(t,x))

    type SqrtOp = SqrtOp with
        static member inline Sqrt(SqrtOp, x : ^T) : ^T = sqrt x
        static member inline Sqrt(SqrtOp, x : ^T) : ^Y = (^T: (member ApplySqrt : unit -> ^Y) (x))
    let inline internal sqrtHelper (t : ^op) (x : ^t) : ^y = ((^op or ^t) : (static member Sqrt : ^op * ^t -> ^y)(t,x))

    type CosOp = CosOp with
        static member inline Cos(CosOp, x : ^T) : ^T = cos x
        static member inline Cos(CosOp, x : ^T) : ^Y = (^T: (member ApplyCos : unit -> ^Y) (x))
    let inline internal cosHelper (t : ^op) (x : ^t) : ^y = ((^op or ^t) : (static member Cos : ^op * ^t -> ^y)(t,x))

    type CoshOp = CoshOp with
        static member inline Cosh(CoshOp, x : ^T) : ^T = cosh x
        static member inline Cosh(CoshOp, x : ^T) : ^Y = (^T: (member ApplyCosh : unit -> ^Y) (x))
    let inline internal coshHelper (t : ^op) (x : ^t) : ^y = ((^op or ^t) : (static member Cosh : ^op * ^t -> ^y)(t,x))

    type SinOp = SinOp with
        static member inline Sin(SinOp, x : ^T) : ^T = sin x
        static member inline Sin(SinOp, x : ^T) : ^Y = (^T: (member ApplySin : unit -> ^Y) (x))
    let inline internal sinHelper (t : ^op) (x : ^t) : ^y = ((^op or ^t) : (static member Sin : ^op * ^t -> ^y)(t,x))

    type SinhOp = SinhOp with
        static member inline Sinh(SinhOp, x : ^T) : ^T = sinh x
        static member inline Sinh(SinhOp, x : ^T) : ^Y = (^T: (member ApplySinh : unit -> ^Y) (x))
    let inline internal sinhHelper (t : ^op) (x : ^t) : ^y = ((^op or ^t) : (static member Sinh : ^op * ^t -> ^y)(t,x))

    type TanOp = TanOp with
        static member inline Tan(TanOp, x : ^T) : ^T = tan x
        static member inline Tan(TanOp, x : ^T) : ^Y = (^T: (member ApplyTan : unit -> ^Y) (x))
    let inline internal tanHelper (t : ^op) (x : ^t) : ^y = ((^op or ^t) : (static member Tan : ^op * ^t -> ^y)(t,x))

    type TanhOp = TanhOp with
        static member inline Tanh(TanhOp, x : ^T) : ^T = tanh x
        static member inline Tanh(TanhOp, x : ^T) : ^Y = (^T: (member ApplyTanh : unit -> ^Y) (x))
    let inline internal tanhHelper (t : ^op) (x : ^t) : ^y = ((^op or ^t) : (static member Tanh : ^op * ^t -> ^y)(t,x))

    type NegateOp = NegateOp with
        static member inline Negate(NegateOp, x : ^T) : ^T = -x
        static member inline Negate(NegateOp, x : ^T) : ^Y = (^T: (member ApplyNegate : unit -> ^Y) (x))
    let inline internal negateHelper (t : ^op) (x : ^t) : ^y = ((^op or ^t) : (static member Negate : ^op * ^t -> ^y)(t,x))

    type PowerOp = PowerOp with
        static member inline Power(PowerOp, x : float, y : float) = System.Math.Pow(x,y)
        static member inline Power(PowerOp, x : float32, y : float32) = System.Math.Pow(double x,double y) |> float32
        static member inline Power(PowerOp, x : ^T, y : ^Y) : ^S = (^S : (static member Pow : ^T * ^Y -> ^S) (x, y))
    let inline internal powerHelper (t : ^op) (x : ^t) (y : ^y) : ^s = ((^op or ^t or ^y) : (static member Power : ^op * ^t * ^y -> ^s)(t,x,y))

open InternalPrimitiveOperatorHelpers

module PrimitiveOperators = 
    let inline exp (x : ^t) : ^y =  expHelper ExpOp x
    let inline log (x : ^t) : ^y =  logHelper LogOp x
    let inline abs (x : ^t) : ^y =  absHelper AbsOp x
    let inline acos (x : ^t) : ^y =  acosHelper AcosOp x
    let inline asin (x : ^t) : ^y =  asinHelper AsinOp x
    let inline atan (x : ^t) : ^y =  atanHelper AtanOp x
    let inline atan2 (x : ^t1) (y : ^t2) : ^y =  atan2Helper AtanOp2 x y
    let inline ceiling (x : ^t) : ^y =  ceilingHelper CeilingOp x
    let inline floor (x : ^t) : ^y =  floorHelper FloorOp x
    let inline truncate (x : ^t) : ^y =  truncateHelper TruncateOp x
    let inline round (x : ^t) : ^y =  roundHelper RoundOp x
    let inline log10 (x : ^t) : ^y =  log10Helper Log10Op x
    let inline sqrt (x : ^t) : ^y =  sqrtHelper SqrtOp x
    let inline cos (x : ^t) : ^y =  cosHelper CosOp x
    let inline cosh (x : ^t) : ^y =  coshHelper CoshOp x
    let inline sin (x : ^t) : ^y =  sinHelper SinOp x
    let inline sinh (x : ^t) : ^y =  sinhHelper SinhOp x
    let inline tan (x : ^t) : ^y =  tanHelper TanOp x
    let inline tanh (x : ^t) : ^y =  tanhHelper TanhOp x
    let inline (~-) (x : ^t) : ^y = negateHelper NegateOp x
    let inline ( ** ) (x : ^t1) (y : ^t2) = powerHelper PowerOp x y    
