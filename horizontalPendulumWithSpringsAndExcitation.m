(* ::Package:: *)

(* ::Title:: *)
(*Horizontal Pendulum with Springs and Excitation*)


(* ::Input:: *)
(*ClearAll["Global`*"]*)
(*$Assumptions={m>0&&Subscript[F, _]>0&&L>0&&kTorsional >0&&kLinear>0}*)


(* ::Section:: *)
(*Equations of Motion*)


(* ::Text:: *)
(*This pendulum is a system of four equal masses:*)


(* ::Input:: *)
(*massQuantity = 4;*)


(* ::Text:: *)
(*Assuming \[Theta] is the angle measured from the horizontal axis. I define position vectors for all masses: *)


(* ::Input:: *)
(*Subscript[r, 1]={L Cos[Subscript[\[Theta], 1][t]],L Sin[Subscript[\[Theta], 1][t]]};*)
(*Subscript[r, 2]={Subscript[r, 1][[1]]+L Cos[Subscript[\[Theta], 2][t]],Subscript[r, 1][[2]]+L Sin[Subscript[\[Theta], 2][t]]};*)
(*Subscript[r, 3]={Subscript[r, 2][[1]]+L Cos[Subscript[\[Theta], 3][t]],Subscript[r, 2][[2]]+L Sin[Subscript[\[Theta], 3][t]]};*)
(*Subscript[r, 4]={Subscript[r, 3][[1]]+L Cos[Subscript[\[Theta], 4][t]],Subscript[r, 3][[2]]+L Sin[Subscript[\[Theta], 4][t]]};*)


(* ::Text:: *)
(*Given that T = 1/2 m v^2, define the kinetic energy of a system:*)


(* ::Input:: *)
(*KE=Sum[1/2 m(D[Subscript[r, k][[1]],t]^2+D[Subscript[r, k][[2]],t]^2),{k,massQuantity}]//FullSimplify*)


(* ::Text:: *)
(*Given Subscript[V, gravity]=m g h, Subscript[V, torsional spring]=1/2 kTorsional \[Theta]^2, Subscript[V, linear spring]=1/2 kLinear x^2. Define potential energy of a system:*)


(* ::Input:: *)
(*PE=Sum[{0,m g}.{ Subscript[r, k][[1]],Subscript[r, k][[2]]},{k,massQuantity}]+Sum[.5kTorsional Subscript[\[Theta], k][t]^2,{k,massQuantity}]+.5 kLinear Subscript[r, 4][[2]]^2//Simplify*)


(* ::Text:: *)
(*Using Equation 6.33 from the book, the Lagrange equation is defined as: *)


(* ::Input:: *)
(*Lg=KE-PE //Simplify*)


(* ::Text:: *)
(*Define virtual work of a system:*)


(* ::Input:: *)
(*\[Delta]W = {0,F[t]}. D[{Subscript[r, 2][[1]],Subscript[r, 2][[2]]},t]/.Derivative[1][Subscript[\[Theta], n_]][t]-> Subscript[\[Delta]\[Theta], n]//Expand*)


(* ::Text:: *)
(*Preallocate memory for the equations of motion:*)


(* ::Input:: *)
(*eq={Null,Null,Null,Null};*)


(* ::Text:: *)
(*Using Equation 6.42 from the book, the equations of motion are:*)


(* ::Input:: *)
(*Scan[*)
(*Print[*)
(*Subscript[eq, #]=Expand[*)
(*D[D[Lg,Derivative[1][Subscript[\[Theta], #]][t]],t]-D[Lg,Subscript[\[Theta], #][t]]==Coefficient[\[Delta]W,Subscript[\[Delta]\[Theta], #]]*)
(*]] &, {1,2,3,4}]*)


(* ::Section:: *)
(*Equilibrium Points*)


(* ::Text:: *)
(*The problem statement defines the static equilibrium when the bars are in the horizontal configuration under the action of gravity and due  to the pretension in both the torsional and linear springs. Thus, thetas at equilibrium are defined as Subscript[\[Theta], 1]=Subscript[\[Theta], 2]=Subscript[\[Theta], 3]=Subscript[\[Theta], 4]=0.*)


(* ::Input:: *)
(*EqRules={Derivative[1][Subscript[\[Theta], _]][t]->0,(Subscript[\[Theta], _]^\[Prime]\[Prime])[t]->0,F[t]->0};*)


(* ::Input:: *)
(*EqEqs={Subscript[eq, 1],Subscript[eq, 2],Subscript[eq, 3],Subscript[eq, 4]}/.EqRules*)


(* ::Section:: *)
(*Linearized Equations of Motion About Equilibrium Points*)


(* ::Text:: *)
(*Prepare for linearization about a general equilibrium point (Subscript[\[Theta]e, 1], Subscript[\[Theta]e, 2], Subscript[\[Theta]e, 3], Subscript[\[Theta]e, 4]) (this is a trick to do multivariate Taylor expansion):*)


(* ::Input:: *)
(*subs=Join[*)
(*Table[Subscript[\[Theta], k][t]-> Subscript[\[Theta]e, k]+q Subscript[\[Theta], k][t],{k,massQuantity}],Table[Subscript[\[Theta], k]'[t]-> q Subscript[\[Theta], k]'[t],{k,massQuantity}],*)
(*Table[Subscript[\[Theta], k]''[t]-> q Subscript[\[Theta], k]''[t],{k,massQuantity}]]*)


(* ::Text:: *)
(*Linearize equations (use the trick):*)


(* ::Input:: *)
(*Do[Print[Subscript[Eq, k]=Normal[Series[Subscript[eq, k]/.subs,{q,0,1}]]/.q-> 1],{k,massQuantity}]*)


(* ::Text:: *)
(*Then the linearized equations of motions about some equilibrium point (Subscript[\[Theta]e, 1], Subscript[\[Theta]e, 2], Subscript[\[Theta]e, 3], Subscript[\[Theta]e, 4]) are:*)


(* ::Input:: *)
(*Do[Print[Subscript[Eq, k]=(Subscript[Eq, k]/.subs)],{k,massQuantity}]*)


(* ::Text:: *)
(*The equations of motion for small oscillations about the stable equilibrium (0,0,0,0) are:*)


(* ::Input:: *)
(*EqMotion=Table[Subscript[Eq, k],{k,massQuantity}]/.{Subscript[\[Theta]e, _]->0}/.q-> 1*)


(* ::Section:: *)
(*Modal Analysis*)


(* ::Text:: *)
(*Use simple parameter assignments as given in the problem:*)


(* ::Input:: *)
(*EqMotion= EqMotion/.kTorsional-> 70 kLinear L^2;*)


(* ::Text:: *)
(*The equations of motion for small oscillations about the stable equilibrium are:*)


(* ::Input:: *)
(*Mm=Normal[CoefficientArrays[EqMotion,Table[Subscript[\[Theta], k]''[t],{k,massQuantity}]]][[2]]/(L^2 m); Mm//MatrixForm*)
(*Km=Normal[CoefficientArrays[EqMotion,Table[Subscript[\[Theta], k][t],{k,massQuantity}]]][[2]]/(L^2 m); Km //MatrixForm*)
(*Fm=EqMotion[[All,2]]/(L);Fm//MatrixForm*)


(* ::Text:: *)
(*Below are the eigenvectors of the system:*)


(* ::Input:: *)
(*EqualMassES=Eigensystem[{m/kLinear Km,Mm}//N]*)


(* ::Section:: *)
(*Natural Frequencies and Normal Modes*)


(* ::Text:: *)
(*Normal modes:*)


(* ::Input:: *)
(*EqualMassNM=EqualMassES[[2]]*)


(* ::Text:: *)
(*Natural frequencies: *)


(* ::Input:: *)
(*EqualMass\[Omega]=Sqrt[EqualMassES[[1]] kLinear/m]//MatrixForm*)


(* ::Text:: *)
(*Plot modes:*)


(* ::Input:: *)
(*plt = Show[ListLinePlot[Table[Thread[{{0,1,2,3,4},Insert[EqualMassNM,0,{{1,1},{2,1},{3,1},{4,1}}][[k]]}],{k,4}],PlotStyle->{Thickness[0.01]},GridLines->Automatic,PlotRange->{{0,4.25},{-1,1}}],ListPlot[Table[Thread[{{1,2,3,4},EqualMassNM[[k]]}],{k,4}],PlotStyle->PointSize[0.03],GridLines->Automatic]]*)


(* ::Section:: *)
(*Frequency Response to Excitations*)


(* ::Text:: *)
(*Derive and plot the frequency response functions for each mass when excited by F(t) = Fe^(i w t) and plot them.*)


(* ::Text:: *)
(*Using equation for 5.97 from the book:*)


(* ::Input:: *)
(*Z\[Omega] = -\[Omega]^2 Mm +m/kLinear Km*)


(* ::Text:: *)
(*Using the method similar to equation 5.100 from the book:*)


(* ::Input:: *)
(*Z\[Omega]Inverse = Inverse[Z\[Omega]]//Simplify;*)


(* ::Text:: *)
(*Using equation 5.99 from the book: *)


(* ::Input:: *)
(*X\[Omega]= Z\[Omega]Inverse.Fm/.F[t]->1//Simplify*)


(* ::Text:: *)
(*The plot of the frequency response function at the first mass:*)


(* ::Input:: *)
(*Plot[X\[Omega][[1]],{\[Omega],-20,20},PlotRange->{-2,2},ExclusionsStyle->Directive[Red,Dashed]]*)


(* ::Text:: *)
(*The plot of the frequency response function at the second mass:*)


(* ::Input:: *)
(*Plot[X\[Omega][[2]],{\[Omega],-20,20},PlotRange->{-2,2},ExclusionsStyle->Directive[Red,Dashed]]*)


(* ::Text:: *)
(*The plot of the frequency response function at the third mass:*)


(* ::Input:: *)
(*Plot[X\[Omega][[3]],{\[Omega],-20,20},PlotRange->{-2,2},ExclusionsStyle->Directive[Red,Dashed]]*)


(* ::Text:: *)
(*The plot of the frequency response function at the fourth mass:*)


(* ::Input:: *)
(*Plot[X\[Omega][[4]],{\[Omega],-20,20},PlotRange->{-2,2},ExclusionsStyle->Directive[Red,Dashed]]*)


(* ::Section:: *)
(*Displacement Amplitudes of the Steady State Response*)


(* ::Text:: *)
(*For \[CapitalOmega] = 0.7Subscript[\[Omega], 2] and \[CapitalOmega] = 0.99Subscript[\[Omega], 2] plot the displacement amplitudes of the steady state response and compare them to the second mode shape.*)


(* ::Text:: *)
(*Using equation 7.199 from the book:*)


(* ::Input:: *)
(*Qt70 = Z\[Omega]Inverse Cos[\[Omega] t]/.\[Omega]-> (.7*(EqualMassES[[1]][[3]])^.5)*)


(* ::Input:: *)
(*Plot[{Qt70[[1]],Qt70[[2]],Qt70[[3]],Qt70[[4]],},{t,0,1.5}]*)


(* ::Text:: *)
(*The behavior of the curves on this plot are very similar to the form of the second mode. There is no damping in the system, so it will continue in this manner indefinitely. *)


(* ::Input:: *)
(*Qt99 = Z\[Omega]Inverse Cos[\[Omega] t]/.\[Omega]-> (.99*(EqualMassES[[1]][[3]])^.5)*)


(* ::Input:: *)
(*Plot[{Qt99[[1]],Qt99[[2]],Qt99[[3]],Qt99[[4]],},{t,0,1.5}]*)


(* ::Text:: *)
(*The behavior of the curves on this plot are unlike the form of the second mode. This is likely due to the fact that the excitation frequency is near a natural frequency, namely the second natural frequency. *)
