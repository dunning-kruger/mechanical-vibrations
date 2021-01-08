(* ::Package:: *)

(* ::Title:: *)
(*Cantilevered Beam With Excitation and Supported By Linear Spring*)


(* ::Input:: *)
(*ClearAll["Global`*"]*)
(*$Assumptions= {\[Beta]>0 && EI>0 &&L>0}*)


(* ::Text:: *)
(*The differential eigenvalue problem without the boundary conditions:*)


(* ::Input:: *)
(*ODE = D[Y[x],{x,4}]-\[Beta]^4 Y[x]==0*)


(* ::Text:: *)
(*General Solution:*)


(* ::Input:: *)
(*sol=Y[x]/.DSolve[ODE,Y[x],x][[1]]//ExpToTrig//Simplify*)


(* ::Text:: *)
(*Thus, we can write this in the following form:*)


(* ::Input:: *)
(*sol=Cosh[x \[Beta]] C[4]+Sinh[x \[Beta]] C[3]+C[2] Cos[x \[Beta]]+C[1] Sin[x \[Beta]]*)


(* ::Input:: *)
(*BCs={*)
(*(sol==0)/.x->0,*)
(*(D[sol,x]==0)/.x->0,*)
(*(EI D[sol,{x,2}]==0)/.x->L,*)
(*(EI D[sol,{x,3}]+k sol==0)/.x->L}*)
(**)


(* ::Input:: *)
(*C4Rule=Solve[BCs[[1]],C[4]][[1]][[1]]*)


(* ::Input:: *)
(*C3Rule=Solve[BCs[[2]]/.C4Rule,C[3]][[1]][[1]]*)


(* ::Input:: *)
(*C2Rule=Solve[BCs[[3]]/.C4Rule/.C3Rule,C[2]][[1]][[1]]*)


(* ::Text:: *)
(*Finally, we use the fourth BC to get the characteristic equation: *)


(* ::Input:: *)
(*charEq = Simplify[BCs[[4]]/.C4Rule/.C3Rule/.C2Rule/.k->.6 EI/L^3/.C[1]->1]*)


(* ::Text:: *)
(*Now, we define a new variable, z:*)


(* ::Input:: *)
(*charEqz=charEq/.L->z/\[Beta]/.EI \[Beta]^3 ->1*)


(* ::Input:: *)
(*Plot[charEqz,{z,0,20},PlotRange->{-3,3}]*)


(* ::Input:: *)
(*roots=FindRoot[charEqz,{z,{1.5,4.5,7.5,10.8,14}}][[1]]*)


(* ::Input:: *)
(*\[Beta]Rule=\[Beta]->z/L/.roots*)


(* ::Input:: *)
(*\[Omega]=z^2 Sqrt[m/EI]/.roots*)


(* ::Text:: *)
(*Now our modes have the form:*)


(* ::Input:: *)
(*\[CapitalPhi][x_]=sol/.C4Rule/.C3Rule/.C2Rule//Simplify*)


(* ::Text:: *)
(*If we plug in the characteristic values, we get all the modes we solved for:*)


(* ::Input:: *)
(*modes=\[CapitalPhi][x]/.\[Beta]Rule*)


(* ::Text:: *)
(*We can normalize them using the norm as:*)


(* ::Input:: *)
(*mags=Chop[Integrate[modes^2,{x,0,L}],10^-6]^(-1/2)*)


(* ::Input:: *)
(*NModes=Simplify[Thread[modes mags],C[1]>0]*)


(* ::Text:: *)
(*Now, we can plot all of them:*)


(* ::Input:: *)
(*Plot[Evaluate[NModes/.L->1],{x,0,1},PlotLegends-> \[Omega]]*)
