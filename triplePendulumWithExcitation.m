(* ::Package:: *)

(* ::Title:: *)
(*Triple Pendulum With Excitation*)


(* ::Input:: *)
(*ClearAll["Global`*"]*)
(*$Assumptions={Subscript[l, _]>0&&Subscript[m, _]>0}*)


(* ::Section:: *)
(*Lagrange's Equations of Motion*)


(* ::Text:: *)
(*Assuming x axis point downward and y axis to the right, we can define position coordinates for all masses :*)


(* ::Input:: *)
(*Subscript[x, 0][t]=0; Do[Print[Subscript[x, k][t_]=Subscript[x, k-1][t]+Subscript[l, k]Cos[Subscript[\[Theta], k][t]]],{k,3}]*)
(*Subscript[y, 0][t]=0; Do[Print[Subscript[y, k][t_]=Subscript[y, k-1][t]+Subscript[l, k]Sin[Subscript[\[Theta], k][t]]],{k,3}]*)


(* ::Text:: *)
(*Define kinetic energy of a system:*)


(* ::Input:: *)
(*T=Sum[1/2 Subscript[m, k ](D[Subscript[x, k][t],t]^2+D[Subscript[y, k][t],t]^2),{k,3}]//Simplify*)


(* ::Text:: *)
(*Define potential energy of a system:*)


(* ::Input:: *)
(*V=Sum[-{Subscript[m, k]g,0}.{ Subscript[x, k][t],Subscript[y, k][t]},{k,3}]//ExpandAll*)


(* ::Input:: *)
(*Lg=T-V*)


(* ::Text:: *)
(*Define virtual work of a system:*)


(* ::Input:: *)
(*\[Delta]W = {0,F[t]}. D[{Subscript[x, 3][t],Subscript[y, 3][t]},t]/.Derivative[1][Subscript[\[Theta], n_]][t]-> Subscript[\[Delta]\[Theta], n]//Expand*)


(* ::Text:: *)
(*Assemble equations of motion:*)


(* ::Input:: *)
(*Scan[Print[*)
(*Subscript[eq, #]=Expand[*)
(*D[D[Lg,Derivative[1][Subscript[\[Theta], #]][t]],t]-D[Lg,Subscript[\[Theta], #][t]]==Coefficient[\[Delta]W,Subscript[\[Delta]\[Theta], #]]*)
(*]] &, {1,2,3}]*)


(* ::Section:: *)
(*Equilibrium Points*)


(* ::Input:: *)
(*EqEqs={Subscript[eq, 1],Subscript[eq, 2],Subscript[eq, 3]}/.{Derivative[1][Subscript[\[Theta], _]][t]->0,(Subscript[\[Theta], _]^\[Prime]\[Prime])[t]->0,F[t]->0}*)


(* ::Input:: *)
(*EqSols=Solve[EqEqs,{Subscript[\[Theta], 1][t],Subscript[\[Theta], 2][t],Subscript[\[Theta], 3][t]}]*)


(* ::Input:: *)
(*EqSols=EqSols/.{C[_]->0}*)


(* ::Text:: *)
(*The only stable equilibrium is the first solution (0,0,0). To see this you can linearize the equations about one of the other equilibrium points (say (0,0,\[Pi])).*)


(* ::Subsection:: *)
(*Linearized Equations of Motion*)


(* ::Text:: *)
(*Prepare for linearization about a general equilibrium point (Subscript[\[Theta]e, 1], Subscript[\[Theta]e, 2], Subscript[\[Theta]e, 3]) (this is a trick to do multivariate Taylor expansion):*)


(* ::Input:: *)
(*subs=Join[*)
(*Table[Subscript[\[Theta], k][t]-> Subscript[\[Theta]e, k]+q Subscript[\[Theta], k][t],{k,3}],Table[Subscript[\[Theta], k]'[t]-> q Subscript[\[Theta], k]'[t],{k,3}],*)
(*Table[Subscript[\[Theta], k]''[t]-> q Subscript[\[Theta], k]''[t],{k,3}]]*)


(* ::Text:: *)
(*Linearize equations (use the trick):*)


(* ::Input:: *)
(*Do[Print[Subscript[Eq, k]=Normal[Series[Subscript[eq, k]/.subs,{q,0,1}]]/.q-> 1],{k,3}]*)


(* ::Text:: *)
(*Use simple parameter assignments as given in the problem:*)


(* ::Input:: *)
(*subs=Join[Table[Subscript[m, k]-> M,{k,3}],Table[Subscript[l, k]-> L,{k,3}]]*)


(* ::Text:: *)
(*Then the linearized equations of motions about some equilibrium point (Subscript[\[Theta]e, 1], Subscript[\[Theta]e, 2], Subscript[\[Theta]e, 3])  are*)


(* ::Input:: *)
(*Do[Print[Subscript[Eq, k]=(Subscript[Eq, k]/.subs)],{k,3}]*)


(* ::Subsection:: *)
(*Stability of Equilibrium Points*)


(* ::Text:: *)
(*To evaluate the stability of the equilibrium points, we need to find the eigenvalues of the Jacobian. If all have nonpositive real parts then the equilibrium is stable, otherwise it is unstable.*)


(* ::Input:: *)
(*Jsols=Solve[{Subscript[Eq, 1],Subscript[Eq, 2],Subscript[Eq, 3]},{(Subscript[\[Theta], 1]^\[Prime]\[Prime])[t],(Subscript[\[Theta], 2]^\[Prime]\[Prime])[t],(Subscript[\[Theta], 3]^\[Prime]\[Prime])[t]}][[1]]/.{F[t]->0}//Simplify*)


(* ::Text:: *)
(*Then the equations of motion can be written is a state space form with respect to a new state variable*)


(* ::Input:: *)
(*z=Join[Table[Subscript[\[Theta], k][t],{k,3}],Table[Subscript[\[Theta], k]'[t],{k,3}]]*)


(* ::Text:: *)
(*The right hand side of the corresponding equation z'=Dz is given by*)


(* ::Input:: *)
(*Dz=Join[z[[4;;6]],Jsols[[1;;3,2]]]*)


(* ::Text:: *)
(*Then, the corresponding Jacobian will be:*)


(* ::Input:: *)
(*JacM=D[Dz,{z}]//Simplify;*)
(*JacM//MatrixForm*)


(* ::Text:: *)
(*To evaluate the stability of equilibrium point, we need to evaluate the Jacobian at each equilibrium and find the corresponding characteristic values. Without the loss of generality we let*)


(* ::Input:: *)
(*JacM = JacM/.{g->9.82,L->1}//Simplify;*)
(*JacM//MatrixForm*)


(* ::Text:: *)
(*Specify each equilibria we want to evaluate:*)


(* ::Input:: *)
(*ERules=EqSols/.{Subscript[\[Theta], p_][t]->Subscript[\[Theta]e, p]}*)


(* ::Text:: *)
(*and get the corresponding eigenvalues*)


(* ::Input:: *)
(*Do[Print[Eigenvalues[JacM/.ERules[[k]]]//Chop],{k,8}]*)


(* ::Text:: *)
(*As we can see only the first equilibrium has eigenvalues with zero real parts, all the others have at least one eigenvalue with positive real part. So, only (0,0,0) equilibrium is stable as we expected.*)


(* ::Section:: *)
(*Modal Analysis*)


(* ::Text:: *)
(*The equations of motion for small oscillations about the stable equilibrium (0,0,0) are:*)


(* ::Input:: *)
(*EqMotion=Table[Subscript[Eq, k],{k,3}]/.{Subscript[\[Theta]e, _]->0}*)


(* ::Text:: *)
(*Identify the corresponding system matrices:*)


(* ::Input:: *)
(* Mm=Normal[CoefficientArrays[EqMotion,Table[Subscript[\[Theta], k]''[t],{k,3}]]][[2]]/(L^2 M); Mm//MatrixForm*)
(*Km=Normal[CoefficientArrays[EqMotion,Table[Subscript[\[Theta], k][t],{k,3}]]][[2]]/(L^2 M); Km//MatrixForm*)
(*Fm=EqMotion[[All,2]]/(L^2 M); Fm//MatrixForm*)


(* ::Text:: *)
(*Find natural frequencies and the modes:*)


(* ::Input:: *)
(*ES=Eigensystem[{L/g Km,Mm}//N]*)


(* ::Text:: *)
(*Natural Frequencies:*)


(* ::Input:: *)
(*\[Omega]=Sqrt[ES[[1]] g/L]*)


(* ::Text:: *)
(*Normal modes:*)


(* ::Input:: *)
(*NM=ES[[2]]*)


(* ::Text:: *)
(*Plot modes:*)


(* ::Input:: *)
(*plt = Show[ListLinePlot[Table[Thread[{{0,1,2,3},Insert[NM,0,{{1,1},{2,1},{3,1}}][[k]]}],{k,3}],PlotStyle->{Thickness[0.01]},GridLines->Automatic,PlotRange->{{0,3.1},{-1,1}}],ListPlot[Table[Thread[{{1,2,3},NM[[k]]}],{k,3}],PlotStyle->PointSize[0.03],GridLines->Automatic]]*)
