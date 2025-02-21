#include "stdafx.h"
#include <iostream>
#include <stdio.h>

#include "AerodynamicModule.h"
#include "AuxVariable.h"
#include "ElasticModule.h"
#include "GeneralModule.h"

void readAndPopulateGeneralData(GeneralData* gd, AeroBody* ab, Wake* w, 
	Spring* s, AuxVariable* av, std::string inputFile)

{
	float temp;
	float temp2;
	int iTemp;
	int iTemp2;

	FILE* pFile;

	pFile = fopen(inputFile.c_str(), "r");
	if (!pFile) {
		perror("File does not exist!!");
	}

	else
	{
		/**************************************/
		/** READ THE GENERAL DATA PARAMETERS **/
		/**************************************/

		fscanf(pFile, "%f", &temp);
		gd->lc = temp;						/// characteristic length

		fscanf(pFile, "%f", &temp);
		gd->vc = temp;						/// characteristic velocity

		fscanf(pFile, "%d", &iTemp);
		gd->nstep = iTemp;					/// number of simulation steps

		fscanf(pFile, "%f", &temp);
		gd->rho = temp;						/// density

		fscanf(pFile, "%f", &temp);
		gd->l_ref = temp;					/// reference length

		fscanf(pFile, "%f", &temp);
		gd->u_inf_x = temp;					/// free stream x-velocity

		fscanf(pFile, "%f", &temp);
		gd->u_inf_y = temp;					/// free stream y-velocity

		fscanf(pFile, "%f", &temp);
		gd->tol = temp;						/// tolerance required for convergence

		fscanf(pFile, "%f", &temp);
		gd->cutoffb = temp;					/// cutoff_b

		fscanf(pFile, "%f", &temp);
		gd->cutoffw = temp;					/// cutoff_w

		fscanf(pFile, "%d", &iTemp);
		gd->nab = iTemp;					/// number of aerdynamic bodies

		fscanf(pFile, "%d", &iTemp);
		gd->naw = iTemp;					/// number of wake emanating bodies

		gd->idabc = new int[iTemp];

		fscanf(pFile, "%d", &iTemp);
		gd->neb = iTemp;					/// number of elastic bodies

		fscanf(pFile, "%d", &iTemp);
		gd->nspring = iTemp;				/// number of springs

		fscanf(pFile, "%d", &iTemp);
		gd->flag_tec = iTemp;				/// tecplot flag

		fscanf(pFile, "%d", &iTemp);
		gd->flag_matlab = iTemp;			/// matlab flag

		fscanf(pFile, "%d", &iTemp);		
		gd->idabc[0] = iTemp;				/// identifying wake bodies #1

		fscanf(pFile, "%d", &iTemp);
		gd->idabc[1] = iTemp;				/// identifying wake bodies #2

		gd->tc = gd->lc / gd->vc;			/// characteristic time
		gd->dt = gd->tc;					/// time step
		
		double mod_uinf = sqrt(gd->u_inf_x * gd->u_inf_x + gd->u_inf_y * gd->u_inf_y);
		gd->x_eD = gd->u_inf_x / mod_uinf;
		gd->y_eD = gd->u_inf_y / mod_uinf;

		gd->x_eL = gd->y_eD;
		gd->y_eL = -gd->x_eD;				/// drag and lift unit vectors

		/*************************************************************/
		/********* GENERAL DATA PARAMETERS READING COMPLETE **********/
		/*************************************************************/

		/********************************************/
		/*** READ THE AERODYNAMIC BODY PARAMETERS ***/
		/********************************************/

		/** first allocate memory for the aerodynamic bodies **/
		for (int i = 0; i <= gd->nab; i++) {
			fscanf(pFile, "%d", &iTemp);
			ab[i]->np = iTemp;						/// number of panels in the first aerodynamic body

			ab->ncp = iTemp;					/// number of control points in the aerodynamic body
			ab->nnp = iTemp + 1;				/// number of nodal points in the aerodynamic body
			ab->nvp = iTemp + 1;				/// number of vortex points in the aerodynamic body
		}
		fscanf(pFile, "%d", &iTemp);
		ab->np = iTemp;						/// number of panels in the first aerodynamic body

		ab->ncp = iTemp;					/// number of control points in the aerodynamic body
		ab->nnp = iTemp + 1;				/// number of nodal points in the aerodynamic body
		ab->nvp = iTemp + 1;				/// number of vortex points in the aerodynamic body

	

	fscanf(pFile, "%f", &temp);

	}

	for (int ind1 = 0; ind1 < nP[0]; ind1++) {
		fscanf(pFile, "%f", &temp);
		particles->x[ind1] = temp;
		//std::cout << '\n' << particles->y[ind1]
	};
	std::cout << "\nIs it still working?";
	//read y values
	for (int ind1 = 0; ind1 < nP[0]; ind1++) {
		fscanf(pFile, "%f", &temp);
		particles->y[ind1] = temp;
		//std::cout << '\n' << particles->y[ind1];
	};

	/*****	NOT READING IN VELOCITY IN THIS VERSION*****
	//read vx values
	for (int ind1=0;ind1<nP[0];ind1++){
		fscanf_s(pFile,"%f",&temp);
		particles->vx[ind1] = temp;
	};

	//read vy values
	for (int ind1=0;ind1<nP[0];ind1++){
		fscanf_s(pFile,"%f",&temp);
		particles->vy[ind1] = temp;
	};
	*/

	std::cout << "\nIs it failing here?";

	//read mass
	/*
	for (int ind1 = 0; ind1 < nP[0]; ind1++) {
		fscanf(pFile, "%f", &temp);
		particles->mass[ind1] = temp;
	};

	//read radius
	for (int ind1 = 0; ind1 < nP[0]; ind1++) {
		fscanf(pFile, "%f", &temp);
		particles->radius[ind1] = temp;
	};
	*/

	//read color
	for (int ind1 = 0; ind1 < nP[0]; ind1++) {
		fscanf(pFile, "%d", &iTemp);
		particles->color[ind1] = iTemp;
	};

	std::cout << "\nFluid particle color: " << particles->color[nP[0] - 1];

	//free particle parameters
	fscanf(pFile, "%f", &temp);
	params->mass = temp;  //maximum fluid velocity, used to determine sound speed; Tait's equation
	std::cout << '\n' << params->mass;

	fscanf(pFile, "%f", &temp);
	params->h = temp;  //viscosity

	fscanf(pFile, "%f", &temp);
	params->cSound = temp;  //speed of sound

	fscanf(pFile, "%f", &temp);
	params->nu = temp;  //viscosity

	fscanf(pFile, "%f", &temp);
	params->delta = temp;  //delta-SPH parameter

	fscanf(pFile, "%f", &temp);
	params->viscoAlpha = temp;  //viscoAlpha

	fscanf(pFile, "%f", &temp);
	params->viscoBeta = temp;  //visco Beta
};