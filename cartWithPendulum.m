(* ::Package:: *)

(* ::Title:: *)
(*Cart With Pendulum*)


(* ::Input:: *)
(*ClearAll["Global`*"]*)
(*$Assumptions={l>0&&k>0&&Subscript[m, 1]>0&&Subscript[m, 2]>0}*)


(* ::Section:: *)
(*Equations of Motion*)


(* ::Text:: *)
(*Position vector of the mass Subscript[m, 2] assuming horizontal x-axis and vertical y-axis. Orientation will be positive x to the right and positive y downwards. *)


(* ::Input:: *)
(*r={x[t]+l Sin[\[Theta][t]],l Cos[\[Theta][t]]}*)


(* ::Text:: *)
(*Potential Energy of the System:*)


(* ::Input:: *)
(*V = 1/2 k x[t]^2+Subscript[m, 2]g r[[2]]*)


(* ::Text:: *)
(*Velocity of the mass Subscript[m, 2]:*)


(* ::Input:: *)
(*v = D[r,t]*)


(* ::Text:: *)
(*Kinetic Energy of the System:*)


(* ::Input:: *)
(*T = 1/2 Subscript[m, 1] x'[t]^2+1/2 Subscript[m, 2]v.v//Simplify//Expand*)


(* ::Text:: *)
(*Virtual Work due to Dissipative and Applied Forces:*)


(* ::Input:: *)
(*\[Delta]W=0*)


(* ::Text:: *)
(*Lagrangian:*)


(* ::Input:: *)
(*L=T-V*)


(* ::Text:: *)
(*Equations of Motion:*)


(* ::Input:: *)
(*EqX=D[D[L,x'[t]],t]-D[L,x[t]]==Coefficient[\[Delta]W,\[Delta]x]*)
(*EqTheta=D[D[L,\[Theta]'[t]],t]-D[L,\[Theta][t]]==Coefficient[\[Delta]W,\[Delta]\[Theta]]*)


(* ::Section:: *)
(*Equilibria*)


(* ::Text:: *)
(*Equilibrium are obtained by setting all the time derivatives to zero in the equations of motion:*)


(* ::Input:: *)
(*EqEqs={EqX,EqTheta}/.{\[Theta]'[t]->0,x'[t]->0,\[Theta]''[t]->0,x''[t]->0}*)


(* ::Text:: *)
(*Now we can solve the above for the equilibrium points:*)


(* ::Input:: *)
(*LinearEqX=Solve[EqEqs[[1]],x[t]][[1]]/. Rule->Equal*)


(* ::Input:: *)
(*LinearEqTheta=Solve[EqEqs[[2]],{\[Theta][t]}]/.Rule->Equal /.{C[_]->0}*)


(* ::Text:: *)
(*Thus, from above we see that (x,\[Theta])  = (0,0) and (0,\[Pi]) and the equilibrium points. *)


(* ::Section:: *)
(*Equilibria*)


(* ::Text:: *)
(*Equations linearized about the equilibrium points are found. Linearization about \[Theta] = 0 result in cos(\[Theta]) -> 1 and sin(\[Theta]) -> \[Theta]. Linearization about \[Theta] = \[Pi] result in cos(\[Theta]) -> -1 and sin(\[Theta]) -> -\[Theta]+\[Pi]. *)


(* ::Input:: *)
(*LinearizedRulesZero={Cos[\[Theta][t]]->1,Sin[\[Theta][t]]->\[Theta]};*)


(* ::Input:: *)
(*LinearizedRulesPi={Cos[\[Theta][t]]->-1,Sin[\[Theta][t]]->-\[Theta]+\[Pi]};*)


(* ::Input:: *)
(*LinearizedEqsZero={EqX,EqTheta}/.LinearizedRulesZero*)


(* ::Input:: *)
(*LinearizedEqsPi={EqX,EqTheta}/.LinearizedRulesPi*)
