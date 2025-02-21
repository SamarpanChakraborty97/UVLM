#pragma once
#ifndef ELASTICMODULE_H
#define ELASTICMODULE_H

struct Spring {

	double khl;			// linear horizontal stiffness of spring
	double khc;			// cubic horizontal stiffness of spring
	double kal;			// linear axial stiffness of spring
	double kac;			// cubic axial stiffness of spring
	double dh;			// horizontal damping coefficient
	double da;			// axial damping coefficient
	double m;			// spring mass
	double ip;			// moment of inertia
	double bb;			// base breadth
	double xa;			// axial position
	double x_fs;		// x-force array
	double y_fs;		// y-force array
	double x_q;			// generalized x-coordinate
	double y_q;			// generalized y-coordinate
	double x_dq;		// generalized x-coordinate derivative
	double y_dq;		// generalized y-coordinate derivative
	double x_R0;		// initial x-position
	double y_R0;		// initial y-position
	double Ceq;			// equivalent capacitance
	double span;		// span of airfoil
	double Rl;			// equivalent load resistance
	double a_coup;		// coupling area
	double voltage;		// voltage
};

#endif // !ELSATICMODULE_H
