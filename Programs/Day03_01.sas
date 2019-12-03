proc import datafile="/rtpnfil02/rtpnfil02_vol6/CSDS/CENTER/Zimmer/AdventOfCode2019/Data/Day03_input.txt" 
dbms=csv replace out=wirepath_in;
getnames=no;
run;

proc transpose data=wirepath_in out=wirepath_t;
var var:;
run;

data Path1;
	set wirepath_t (keep=col1 rename=(col1=Instruction));
	retain x y step 0;

	Dir=substr(Instruction, 1, 1);
	Dist=input(substr(Instruction, 2), 8.);
	do i = 1 to Dist;
		Step=Step+1;
		if Dir="U" then y=y+1;
		else if Dir="D" then y=y-1;
		else if Dir="L" then x=x-1;
		else if Dir="R" then x=x+1;
		output;
	end;
	keep x y step;
	rename Step=Step1;
run;

data Path2;
	set wirepath_t (keep=col2 rename=(col2=Instruction));
	retain x y step 0;

	Dir=substr(Instruction, 1, 1);
	Dist=input(substr(Instruction, 2), 8.);
	do i = 1 to Dist;
		Step=Step+1;
		if Dir="U" then y=y+1;
		else if Dir="D" then y=y-1;
		else if Dir="L" then x=x-1;
		else if Dir="R" then x=x+1;
		output;
	end;
	keep x y step;
	rename Step=Step2;
run;

proc sort data=Path1;
by x y;
run;
proc sort data=Path2;
by x y;
run;

data PathJoin;
	merge Path1 (in=a) Path2 (in=b);
	by x y;
	if a and b;
run;

data PathJoinDist;
	set PathJoin;
	Dist=abs(x)+abs(y);
	TotSteps=sum(step1, step2);
run;

title "Solution to Part 1 and Part 2 of Day 3";
proc means data=PathJoinDist min;
var Dist TotSteps;
run;
title;
