// "Optimised" varient of PG's Mahajan model, convert to C11

// Standard use libraries
// ======================
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

// CVODE Libraries
// ===============
#include "cvode/cvode.h"				/* Prototypes for CVODE fcts., consts. */
#include "nvector/nvector_serial.h"		/* Serial N_Vector types, fcts., macros */
#include "cvode/cvode_dense.h"			/* Prototype for CVDense */
#include "sundials/sundials_dense.h"	/* Definitions DlsMat DENSE_ELEM */
#include "sundials/sundials_types.h"	/* Definition of type realtype */

// Ode FFI
#include "OdeStdLib.h"

// CVODE Set-up
// ============
#define stim_length	RCONST(1000000.0)	/* Total simulation time */
//#define bcl		RCONST(1000.0)			/* BCL */
//#define bcl_number	2					/* Number of APs to be recorded */

//#define offset		stim_length-floor(stim_length/bcl)*bcl

#define Ith(v,i)	NV_Ith_S(v,i-1)			/* Ith numbers components 1...NEQ */
//#define IJth(A,i,j)	DENSE_ELEM(A,i-1,j-1)	/* IJth numbers rows,cols 1...NEQ */
#define NEQ			33						/* Number of equations (plus six ion channel currents) */
#define T0			RCONST(0.0)				/* Initial time */
#define out_step	RCONST(10)				/* Output time step */
#define RTOL		RCONST(1e-9)			/* Scalar relative tolerance (original = 1e-7) */
#define ATOL		RCONST(1e-11)			/* Vector absolute tolerance components (original = 1e-9) */

//#define T1		stim_length-(2*bcl)		/* First output time */
//#define NOUT	(2*bcl)/out_step		/* Number of output times */

// Problem Constants
// =================

#define Y1 104.108060320073;		/* Ca_NSR (uM) (in Ca) */
#define Y2 1.91267853216796;		/* Ca_dyad (uM) (in Ca) */
#define Y3 0.216591220613237;		/* Ca_i (uM) (in Ca) */
#define Y4 0.206588249441256;		/* Ca_submem (uM) (in Ca) */
#define Y5 19.3132456227266;		/* tropi (uM) (in Ca) */
#define Y6 18.1964870238395;		/* trops (uM) (in Ca) */
#define Y7 1.77678273893783e-5;		/* c1 (dimensionless) (in ICaL) */
#define Y8 0.981806904399343;		/* c2 (dimensionless) (in ICaL) */
#define Y9 3.28810184500228e-5;		/* xi1ba (dimensionless) (in ICaL) */
#define Y10 0.000667715244070818;	/* xi1ca (dimensionless) (in ICaL) */
#define Y11 0.0150131872489933;		/* xi2ba (dimensionless) (in ICaL) */
#define Y12 0.0024597678895653;		/* xi2ca (dimensionless) (in ICaL) */
#define Y13 0.00684293689712338;	/* xr (dimensionless) (in IKr) */
#define Y14 0.0256917227076164;		/* xs1 (dimensionless) (in IKs) */
#define Y15 0.0769816106642697;		/* xs2 (dimensionless) (in IKs) */
#define Y16 0.991065948286682;		/* xh (dimensionless) (in INa) */
#define Y17 0.994131546457503;		/* xj (dimensionless) (in INa) */
#define Y18 0.00104347579348688;	/* xm (dimensionless) (in INa) */
#define Y19 99.4448177355167;		/* Ca_JSR (uM) (in Irel) */
#define Y20 0.00821527423360247;	/* xir (uM_per_ms) (in Irel) */
#define Y21 0.00359982129123271;	/* xtof (dimensionless) (in Ito) */
#define Y22 0.00360113747215906;	/* xtos (dimensionless) (in Ito) */
#define Y23 0.995225170570617;		/* ytof (dimensionless) (in Ito) */
#define Y24 0.201006999888848;		/* ytos (dimensionless) (in Ito) */
#define Y25 11.3241630632199;		/* Na_i (mM) (in Na) */
#define Y26 -87.349438978933;		/* V (mV) (in cell) */
#define Y27 144.473230653346;		/* K_i (mM) (in ionic_concentrations) */
#define Y28	0.0						/* xica */
#define Y29	0.0						/* xikr */
#define Y30	0.0						/* xiks */
#define Y31	0.0						/* xik1 */
#define Y32	0.0						/* xiNaK */
#define Y33	0.0						/* xito */


/* Private helper function */
static int f(realtype t, N_Vector y, N_Vector ydot, void* f_data);
static int check_flag(void *flagvalue, char *funcname, int opt);

int main(void)
{

	realtype reltol, t, tout;
	N_Vector y, abstol;
	void *cvode_mem;
    int flag;

    // Ode setup
    OdeInit();
    OdeStartSim("output.bin", NEQ);

	/* Allocate y, abstol vectors, and create serial vectors of length NEQ for I.C. and abstol */
	y = abstol = NULL;		cvode_mem = NULL;

	y = N_VNew_Serial(NEQ);
	if (check_flag((void *)y, "N_VNew_Serial", 0))
	{
		return(1);
	}
	abstol = N_VNew_Serial(NEQ);
	if (check_flag((void *)abstol, "N_VNew_Serial", 0))
	{
		return(1);
	}

	/* Initialise y */
	Ith(y,1) = Y1;		Ith(y,2) = Y2;		Ith(y,3) = Y3;		Ith(y,4) = Y4;		Ith(y,5) = Y5;
	Ith(y,6) = Y6;		Ith(y,7) = Y7;		Ith(y,8) = Y8;		Ith(y,9) = Y9;		Ith(y,10) = Y10;
	Ith(y,11) = Y11;	Ith(y,12) = Y12;	Ith(y,13) = Y13;	Ith(y,14) = Y14;	Ith(y,15) = Y15;
	Ith(y,16) = Y16;	Ith(y,17) = Y17;	Ith(y,18) = Y18;	Ith(y,19) = Y19;	Ith(y,20) = Y20;
	Ith(y,21) = Y21;	Ith(y,22) = Y22;	Ith(y,23) = Y23;	Ith(y,24) = Y24;	Ith(y,25) = Y25;
	Ith(y,26) = Y26;	Ith(y,27) = Y27;	Ith(y,28) = Y28;	Ith(y,29) = Y29;	Ith(y,30) = Y30;
	Ith(y,31) = Y31;	Ith(y,32) = Y32;	Ith(y,33) = Y33;

    // write inital vales to disk
    OdeWriteState(T0, NV_DATA_S(y));

	/* Set the error tolerances */
	reltol = RTOL;
	for (int i=1; i<=NEQ; i++)
	{
		Ith(abstol,i) = ATOL;
	}

	/* Call CVodeCreate to create the solver memory and specify the Backward Differentiation Formula and the use of a Newton iteration */
	cvode_mem = CVodeCreate(CV_BDF, CV_NEWTON);
	if (check_flag((void *)cvode_mem, "CVodeCreate", 0))
	{
		return(1);
	}

	/* Call CVodeInit to initialize the integrator memory and specify the user's right hand side function in y'=f(t,y), the inital time T0, and the initial dependent variable vector y. */
	flag = CVodeInit(cvode_mem, f, T0, y);
	if (check_flag(&flag, "CVodeInit", 1))
	{
		return(1);
	}

	/* Call CVodeSVtolerances to specify the scalar relative tolerance and vector absolute tolerances */
	flag = CVodeSVtolerances(cvode_mem, reltol, abstol);
	if (check_flag(&flag, "CVodeSVtolerances", 1))
	{
		return(1);
	}

	/* Alter the number of maximum steps the solver can take before it reaches tout */
    flag = CVodeSetMaxNumSteps(cvode_mem, 10000);

	/* Set the maximum/minimum step size the solver may take, such that the stimulus current will not be missed, and the initial step size */
    // set to 1/2 the stimulus period - 1.5
    //	flag = CVodeSetMaxStep(cvode_mem, 3.0);
    flag = CVodeSetMaxStep(cvode_mem, 1.5);
    //flag = CVodeSetMinStep(cvode_mem, 0.0001);
	//flag = CVodeSetInitStep(cvode_mem, 0.001);

	/* Set the maximum number of error test fails that may be taken (default = 7) */
	flag = CVodeSetMaxErrTestFails(cvode_mem, 25);

	/* Set the number of error messages that t = t+h will be issued (default = 10, negative means no error messages will be given) */
	flag = CVodeSetMaxHnilWarns(cvode_mem, -1);

	/* Set the maximum number of nonlinear solver convergence failures permitted during one step (default = 10) */
	flag = CVodeSetMaxConvFails(cvode_mem, 250);

	/* Call CVDense to specify the CVDENSE dense linear solver */
	flag = CVDense(cvode_mem, NEQ);
	if (check_flag(&flag, "CVDense", 1))
	{
		return(1);
	}

	//flag = CVodeMalloc(cvode_mem, f, T0, y0, CV_SV, reltol, abstol);

    // setup timeing params
    flag = CVodeSetStopTime(cvode_mem, stim_length);
    uint64_t curLoop = 0;
    tout = out_step;
	while(1)
	{
        ++curLoop;
        tout = T0 + curLoop * out_step;
        flag = CVode(cvode_mem, tout, y, &t, CV_NORMAL);

        if (flag == CV_TSTOP_RETURN) break;
        if (check_flag(&flag, "CVode", 1))
		{
			break;
		}

        OdeWriteState(t, NV_DATA_S(y));
	}

	if (flag == CV_SUCCESS)
	{
        puts("Simulation complete!");
	}
	else if (flag != CV_SUCCESS)
	{
        puts("Simulation failed!");
	}

	/* Free y and abstol vectors */
	N_VDestroy_Serial(y);
	N_VDestroy_Serial(abstol);

	/* Free integrator memory */
	CVodeFree(&cvode_mem);

    // Ode Shutdown
    OdeStopSim();
    OdeShutdown();

	return(0);
}

static int f(realtype t, N_Vector y, N_Vector ydot, void *f_data)
{

	//---------------------------------------------------------------------------
	// Constants
	//---------------------------------------------------------------------------

	double bcal = 24.0;		/* uM (in Ca) */	double bmem = 15.0;		/* uM (in Ca) */	double bsar = 42.0;		/* uM (in Ca) */
	double btrop = 70.0;	/* uM (in Ca) */	double kmem = 0.3;		/* uM (in Ca) */	double ksar = 13.0;		/* uM (in Ca) */
	double srkd = 0.6;		/* uM (in Ca) */	double srmax = 47.0;	/* uM (in Ca) */	double taud = 4.0;		/* ms (in Ca) */
	double taups = 0.5;		/* ms (in Ca) */	double xkcal = 7.0;		/* uM (in Ca) */	double xkoff = 0.0196;	/* per_ms (in Ca) */
	double xkon = 0.0327;	/* per_uM_per_ms (in Ca) */

	double Ca_o = 1.8;		/* mM (in Environment) */			double F = 96.4853415;	/* coulomb_per_mmole (in Environment) */
	double K_o = 5.4;		/* mM (in Environment) */			double Na_o = 136.0;	/* mM (in Environment) */
	double R = 8.314472;	/* J_per_moleK (in Environment) */	double T = 308.0;		/* kelvin (in Environment) */

	double cat = 3.0;		/* uM (in ICaL) */					double cpt = 6.09365;	/* uM (in ICaL) */
	double gca = 182.0;		/* mmole_per_coulomb_cm (in ICaL) */double k1t = 0.00413;	/* per_ms (in ICaL) */
	double k2 = 0.000103615;/* per_ms (in ICaL) */				double k2t = 0.00224;	/* per_ms (in ICaL) */
	double pca = 0.00054;	/* cm_per_s (in ICaL) */			double r1 = 0.3;		/* per_ms (in ICaL) */
	double r2 = 3.0;		/* per_ms (in ICaL) */				double s1t = 0.00195;	/* per_ms (in ICaL) */
	double s6 = 8.0;		/* mV (in ICaL) */					double sx = 3.0;		/* mV (in ICaL) */
	double sy = 4.0;		/* mV (in ICaL) */					double syr = 11.32;		/* mV (in ICaL) */
	double tau3 = 3.0;		/* ms (in ICaL) */					double taupo = 1.0;		/* ms (in ICaL) */
	double tca = 78.0329;	/* ms (in ICaL) */					double vth = 0.0;		/* mV (in ICaL) */
	double vx = -40.0;		/* mV (in ICaL) */					double vy = -40.0;		/* mV (in ICaL) */
	double vyr = -40.0;		/* mV (in ICaL) */

	double gkix = 0.3;		/* uS_per_nF (in IK1) */

	double gkr = 0.0125;	/* uS_per_nF (in IKr) */

	double gks = 0.1386;	/* uS_per_nF (in IKs) */

	double gNaCa = 0.84;	/* uM_per_ms (in INaCa) */			double xkdna = 0.3;		/* uM (in INaCa) */
	double xmcai = 0.0036;	/* mM (in INaCa) */					double xmcao = 1.3;		/* mM (in INaCa) */
	double xmnai = 12.3;	/* mM (in INaCa) */					double xmnao = 87.5;	/* mM (in INaCa) */

	double gNaK = 1.5;		/* nA_per_nF (in INaK) */			double xkmko = 1.5;		/* mM (in INaK) */
	double xkmnai = 12.0;	/* mM (in INaK) */

	double gna = 12.0;		/* uS_per_nF (in INa) */

	double A_atp = 2.0;		/* mM (in Ikatp) */					double K_atp = 0.25;	/* mM (in Ikatp) */
	double K_o_n = 5.4;		/* mM (in Ikatp) */					double fkatp = 0.05;	/* dimensionless (in Ikatp) */
	double gkatp = 4.4;		/* uS_per_nF (in Ikatp) */

	double cup = 0.5;		/* uM (in Ileak_Iup_Ixfer) */		double gleak = 2.069e-5;/* per_ms (in Ileak_Iup_Ixfer) */
	double kj = 50.0;		/* uM (in Ileak_Iup_Ixfer) */		double vup = 0.4;		/* uM_per_ms (in Ileak_Iup_Ixfer) */

	double av = 11.3;		/* per_ms (in Irel) */				double ax = 0.3576;		/* per_mV (in Irel) */
	double ay = 0.05;		/* per_mV (in Irel) */				double cstar = 90.0;	/* uM (in Irel) */
	double gbarsr = 26841.8;/* dimensionless (in Irel) */		double gdyad = 9000.0;	/* mmole_per_coulomb_cm (in Irel) */
	double gryr = 2.58079;	/* per_ms (in Irel) */				double taua = 100.0;	/* ms (in Irel) */
	double taur = 30.0;		/* ms (in Irel) */

	double gto = 1.0;		/* dimensionless (in Ito) */		double gtof = 0.11;		/* uS_per_nF (in Ito) */
	double gtos = 0.04;		/* uS_per_nF (in Ito) */

	double stim_amplitude = -15.0;	/* nA_per_nF (in cell) */	double stim_duration = 3.0;		/* ms (in cell) */
	double stim_offset = 0.0;		/* ms (in cell) */			double stim_period = 400.0;		/* ms (in cell) */
	double wca = 8.0;		/* mV_per_uM (in cell) */

	double prNaK = 0.01833;	/* dimensionless (in reversal_potentials) */

	//------------------------------------------------------------------------
	// Computed variables
	//------------------------------------------------------------------------

	double bpxi;	/* dimensionless (in Ca) */		double bpxs;	/* dimensionless (in Ca) */
	double csm;		/* mM (in Ca) */				double dCa_JSR;	/* uM_per_ms (in Ca) */
	double dciib;	/* dimensionless (in Ca) */		double dcsib;	/* dimensionless (in Ca) */
	double jd;		/* uM_per_ms (in Ca) */			double mempxi;	/* dimensionless (in Ca) */
	double mempxs;	/* dimensionless (in Ca) */		double sarpxi;	/* dimensionless (in Ca) */
	double sarpxs;	/* dimensionless (in Ca) */		double spxi;	/* dimensionless (in Ca) */
	double spxs;	/* dimensionless (in Ca) */		double xbi;		/* uM_per_ms (in Ca) */
	double xbs;		/* uM_per_ms (in Ca) */

	double FonRT;	/* per_mV (in Environment) */

	double Pr;		/* dimensionless (in ICaL) */	double Ps;		/* dimensionless (in ICaL) */
	double alpha;	/* per_ms (in ICaL) */			double beta;	/* per_ms (in ICaL) */
	double fca;		/* dimensionless (in ICaL) */	double jca;		/* uM_per_ms (in ICaL) */
	double k1;		/* per_ms (in ICaL) */			double k3;		/* per_ms (in ICaL) */
	double k3t;		/* per_ms (in ICaL) */			double k4;		/* per_ms (in ICaL) */
	double k4t;		/* per_ms (in ICaL) */			double k5;		/* per_ms (in ICaL) */
	double k5t;		/* per_ms (in ICaL) */			double k6;		/* per_ms (in ICaL) */
	double k6t;		/* per_ms (in ICaL) */			double po;		/* dimensionless (in ICaL) */
	double poi;		/* dimensionless (in ICaL) */	double poinf;	/* dimensionless (in ICaL) */
	double recov;	/* ms (in ICaL) */				double rxa;		/* mA_per_cm2 (in ICaL) */
	double s1;		/* per_ms (in ICaL) */			double s2;		/* per_ms (in ICaL) */
	double s2t;		/* per_ms (in ICaL) */			double tau_ca;	/* ms (in ICaL) */
	double tauba;	/* ms (in ICaL) */				double tauca;	/* ms (in ICaL) */
	double xica;	/* nA_per_nF (in ICaL) */		double za;		/* dimensionless (in ICaL) */

	double aki;		/* per_ms (in IK1) */			double bki;		/* per_ms (in IK1) */
	double xik1;	/* nA_per_nF (in IK1) */		double xkin;	/* dimensionless (in IK1) */

	double rg;		/* dimensionless (in IKr) */	double taukr;	/* ms (in IKr) */
	double xikr;	/* nA_per_nF (in IKr) */		double xkrinf;	/* dimensionless (in IKr) */
	double xkrv1;	/* per_ms (in IKr) */			double xkrv2;	/* per_ms (in IKr) */

	double gksx;	/* dimensionless (in IKs) */	double tauxs1;	/* ms (in IKs) */
	double tauxs2;	/* ms (in IKs) */				double xiks;	/* nA_per_nF (in IKs) */
	double xs1ss;	/* dimensionless (in IKs) */	double xs2ss;	/* dimensionless (in IKs) */

	double aloss;	/* dimensionless (in INaCa) */	double jNaCa;	/* uM_per_ms (in INaCa) */
	double xiNaCa;	/* nA_per_nF (in INaCa) */		double yz1;		/* mM4 (in INaCa) */
	double yz2;		/* mM4 (in INaCa) */			double yz3;		/* mM4 (in INaCa) */
	double yz4;		/* mM4 (in INaCa) */			double zw3;		/* mM4 (in INaCa) */
	double zw4;		/* dimensionless (in INaCa) */	double zw8;		/* mM4 (in INaCa) */

	double fNaK;	/* dimensionless (in INaK) */	double sigma;	/* dimensionless (in INaK) */
	double xiNaK;	/* nA_per_nF (in INaK) */

	double ah;		/* per_ms (in INa) */			double aj;		/* per_ms (in INa) */
	double am;		/* per_ms (in INa) */			double bh;		/* per_ms (in INa) */
	double bj;		/* per_ms (in INa) */			double bm;		/* per_ms (in INa) */
	double xina;	/* nA_per_nF (in INa) */

	double akik;	/* dimensionless (in Ikatp) */	double bkik;	/* dimensionless (in Ikatp) */
	double xika;	/* nA_per_nF (in Ikatp) */

	double jleak;	/* uM_per_ms (in Ileak_Iup_Ixfer) */	double jup;		/* uM_per_ms (in Ileak_Iup_Ixfer) */

	double Qr;		/* uM_per_ms (in Irel) */		double Qr0;		/* uM_per_ms (in Irel) */
	double bv;		/* uM_per_ms (in Irel) */		double sparkV;	/* dimensionless (in Irel) */
	double spark_rate;	/* per_ms (in Irel) */		double xicap;	/* uM_per_ms (in Irel) */
	double xirp;	/* uM_per_ms (in Irel) */		double xiryr;	/* uM_per_ms (in Irel) */

	double rs_inf;	/* dimensionless (in Ito) */	double rt1;		/* dimensionless (in Ito) */
	double rt2;		/* dimensionless (in Ito) */	double rt3;		/* dimensionless (in Ito) */
	double rt4;		/* dimensionless (in Ito) */	double rt5;		/* dimensionless (in Ito) */
	double txf;		/* ms (in Ito) */				double txs;		/* ms (in Ito) */
	double tyf;		/* ms (in Ito) */				double tys;		/* ms (in Ito) */
	double xito;	/* nA_per_nF (in Ito) */		double xitof;	/* nA_per_nF (in Ito) */
	double xitos;	/* nA_per_nF (in Ito) */		double xtof_inf;/* dimensionless (in Ito) */
	double xtos_inf;/* dimensionless (in Ito) */	double ytof_inf;/* dimensionless (in Ito) */
	double ytos_inf;/* dimensionless (in Ito) */

	double Itotal;	/* nA_per_nF (in cell) */		double i_Stim;	/* nA_per_nF (in cell) */
	double past;	/* ms (in cell) */

	double ek;		/* mV (in reversal_potentials) */	double eks;		/* mV (in reversal_potentials) */
	double ena;		/* mV (in reversal_potentials) */

	//--------------------------------------------------------------------------
	// Scaling parameters
	//--------------------------------------------------------------------------

	double ScaleFactorGCaL = 1.0;	/* dimensionless (in ICaL) */	double ScaleFactorGKr = 1.0;		/* dimensionless (in IKr) */
	double ScaleFactorGKs = 1.0;	/* dimensionless (in IKs) */	double ScaleFactorGK1 = 1.0;		/* dimensionless (in IK1) */
	double ScaleFactorGNaK = 1.0;	/* dimensionless (in INaK) */	double ScaleFactorGto = 1.0;		/* dimensionless (in Ito) */

	//---------------------------------------------------------------------------
	// Computed variables
	//---------------------------------------------------------------------------

	bv = (1.0-av)*cstar-50.0;
	FonRT = F/(R*T);
	s2t = s1t*r1/r2*k2t/k1t;
	sigma = (exp(Na_o/67.3)-1.0)/7.0;
	akik = pow(K_o/K_o_n, 0.24);
	bkik = 1.0/(1.0+pow(A_atp/K_atp, 2.0));

	//------------------------------------------------------------------------------
	// Computation segment of code
	//------------------------------------------------------------------------------

	bpxs = bcal*xkcal/((xkcal+Ith(y,4))*(xkcal+Ith(y,4)));
	spxs = srmax*srkd/((srkd+Ith(y,4))*(srkd+Ith(y,4)));
	mempxs = bmem*kmem/((kmem+Ith(y,4))*(kmem+Ith(y,4)));
	sarpxs = bsar*ksar/((ksar+Ith(y,4))*(ksar+Ith(y,4)));
	dcsib = 1.0/(1.0+bpxs+spxs+mempxs+sarpxs);
	bpxi = bcal*xkcal/((xkcal+Ith(y,3))*(xkcal+Ith(y,3)));
	spxi = srmax*srkd/((srkd+Ith(y,3))*(srkd+Ith(y,3)));
	mempxi = bmem*kmem/((kmem+Ith(y,3))*(kmem+Ith(y,3)));
	sarpxi = bsar*ksar/((ksar+Ith(y,3))*(ksar+Ith(y,3)));
	dciib = 1.0/(1.0+bpxi+spxi+mempxi+sarpxi);
	jd = (Ith(y,4)-Ith(y,3))/taud;
	xbi = xkon*Ith(y,3)*(btrop-Ith(y,5))-xkoff*Ith(y,5);
	xbs = xkon*Ith(y,4)*(btrop-Ith(y,6))-xkoff*Ith(y,6);
	jup = vup*Ith(y,3)*Ith(y,3)/(Ith(y,3)*Ith(y,3)+cup*cup);
	jleak = gleak*Ith(y,1)*Ith(y,1)/(Ith(y,1)*Ith(y,1)+kj*kj)*(Ith(y,1)*16.667-Ith(y,3));
	dCa_JSR = -Ith(y,20)+jup-jleak;
	po = 1.0-Ith(y,10)-Ith(y,12)-Ith(y,9)-Ith(y,11)-Ith(y,7)-Ith(y,8);

	if ((Ith(y,19) > 50.0) && (Ith(y,19) < cstar))
	{
		Qr0 = (Ith(y,19)-50.0)/1.0;
	}
	else if (Ith(y,19) >= cstar)
	{
		Qr0 = av*Ith(y,19)+bv;
	}
	else
	{
		Qr0 = 0.0;
	}

	Qr = Ith(y,1)*Qr0/cstar;
	csm = Ith(y,4)/1000.0;
	za = Ith(y,26)*2.0*FonRT;

	if (fabs(za) < 0.001)
	{
		rxa = 4.0*pca*F*FonRT*(csm*exp(za)-0.341*Ca_o)/(2.0*FonRT);
	}
	else
	{
		rxa = 4.0*pca*Ith(y,26)*F*FonRT*(csm*exp(za)-0.341*Ca_o)/(exp(za)-1.0);
	}

	xirp = po*Qr*fabs(rxa)*gbarsr/1.0*exp(-ax*(Ith(y,26)+30.0))/(1.0+exp(-ax*(Ith(y,26)+30.0)));
	xicap = po*gdyad*fabs(rxa);
	xiryr = xirp+xicap;
	Ith(ydot,2) = xiryr-(Ith(y,2)-Ith(y,4))/taups;
	jca = ScaleFactorGCaL*gca*po*rxa;
	aloss = 1.0/(1.0+pow(xkdna/Ith(y,4), 3.0));
	zw3 = pow(Ith(y,25), 3.0)*Ca_o*exp(Ith(y,26)*0.35*FonRT)-pow(Na_o, 3.0)*csm*exp(Ith(y,26)*(0.35-1.0)*FonRT);
	zw4 = 1.0+0.2*exp(Ith(y,26)*(0.35-1.0)*FonRT);
	yz1 = xmcao*pow(Ith(y,25), 3.0)+pow(xmnao, 3.0)*csm;
	yz2 = pow(xmnai, 3.0)*Ca_o*(1.0+csm/xmcai);
	yz3 = xmcai*pow(Na_o, 3.0)*(1.0+pow(Ith(y,25)/xmnai, 3.0));
	yz4 = pow(Ith(y,25), 3.0)*Ca_o+pow(Na_o, 3.0)*csm;
	zw8 = yz1+yz2+yz3+yz4;
	jNaCa = gNaCa*aloss*zw3/(zw4*zw8);
	Ith(ydot,4) = dcsib*(50.0*(Ith(y,20)-jd-jca+jNaCa)-xbs);
	Ith(ydot,3) = dciib*(jd-jup+jleak-xbi);
	Ith(ydot,1) = dCa_JSR;
	Ith(ydot,5) = xbi;
	Ith(ydot,6) = xbs;
	poinf = 1.0/(1.0+exp(-(Ith(y,26)-vth)/s6));
	alpha = poinf/taupo;
	beta = (1.0-poinf)/taupo;
	fca = 1.0/(1.0+pow(cat/Ith(y,2), 3.0));
	s1 = 0.0182688*fca;
	k1 = 0.024168*fca;
	s2 = s1*r1/r2*k2/k1;
	poi = 1.0/(1.0+exp(-(Ith(y,26)-vx)/sx));
	k3 = (1.0-poi)/tau3;
	k3t = k3;
	Pr = 1.0-1.0/(1.0+exp(-(Ith(y,26)-vy)/sy));
	recov = 10.0+4954.0*exp(Ith(y,26)/15.6);
	tau_ca = tca/(1.0+pow(Ith(y,2)/cpt, 4.0))+0.1;
	tauca = (recov-tau_ca)*Pr+tau_ca;
	tauba = (recov-450.0)*Pr+450.0;
	Ps = 1.0/(1.0+exp(-(Ith(y,26)-vyr)/syr));
	k6 = fca*Ps/tauca;
	k5 = (1.0-Ps)/tauca;
	k6t = Ps/tauba;
	k5t = (1.0-Ps)/tauba;
	k4 = k3*alpha/beta*k1/k2*k5/k6;
	k4t = k3t*alpha/beta*k1t/k2t*k5t/k6t;
	Ith(ydot,7) = alpha*Ith(y,8)+k2*Ith(y,10)+k2t*Ith(y,9)+r2*po-(beta+r1+k1t+k1)*Ith(y,7);
	Ith(ydot,8) = beta*Ith(y,7)+k5*Ith(y,12)+k5t*Ith(y,11)-(k6+k6t+alpha)*Ith(y,8);
	Ith(ydot,10) = k1*Ith(y,7)+k4*Ith(y,12)+s1*po-(k3+k2+s2)*Ith(y,10);
	Ith(ydot,9) = k1t*Ith(y,7)+k4t*Ith(y,11)+s1t*po-(k3t+k2t+s2t)*Ith(y,9);
	Ith(ydot,12) = k3*Ith(y,10)+k6*Ith(y,8)-(k5+k4)*Ith(y,12);
	Ith(ydot,11) = k3t*Ith(y,9)+k6t*Ith(y,8)-(k5t+k4t)*Ith(y,11);
	xica = 2.0*wca*jca;
	Ith(y,28) = xica;
	ek = 1.0/FonRT*log(K_o/Ith(y,27));
	aki = 1.02/(1.0+exp(0.2385*(Ith(y,26)-ek-59.215)));
	bki = (0.49124*exp(0.08032*(Ith(y,26)-ek+5.476))+1.0*exp(0.06175*(Ith(y,26)-ek-594.31)))/(1.0+exp(-0.5143*(Ith(y,26)-ek+4.753)));
	xkin = aki/(aki+bki);
	xik1 = ScaleFactorGK1*gkix*sqrt(K_o/5.4)*xkin*(Ith(y,26)-ek);
	Ith(y,31) = xik1;

	if (fabs(Ith(y,26)+7.0) > 0.001)
	{
		xkrv1 = 0.00138*1.0*(Ith(y,26)+7.0)/(1.0-exp(-0.123*(Ith(y,26)+7.0)));
	}
	else
	{
		xkrv1 = 0.00138/0.123;
	}

	if (fabs(Ith(y,26)+10.0) > 0.001)
	{
		xkrv2 = 0.00061*1.0*(Ith(y,26)+10.0)/(exp(0.145*(Ith(y,26)+10.0))-1.0);
	}
	else
	{
		xkrv2 = 0.00061/0.145;
	}

	taukr = 1.0/(xkrv1+xkrv2);
	xkrinf = 1.0/(1.0+exp(-(Ith(y,26)+50.0)/7.5));
	rg = 1.0/(1.0+exp((Ith(y,26)+33.0)/22.4));
	xikr = ScaleFactorGKr*gkr*sqrt(K_o/5.4)*Ith(y,13)*rg*(Ith(y,26)-ek);
	Ith(y,29) = xikr;
	Ith(ydot,13) = (xkrinf-Ith(y,13))/taukr;
	xs1ss = 1.0/(1.0+exp(-(Ith(y,26)-1.5)/16.7));
	xs2ss = xs1ss;

	if (fabs(Ith(y,26)+30.0) < 0.001/0.0687)
	{
		tauxs1 = 1.0/(0.0000719/0.148+0.000131/0.0687);
	}
	else
	{
		tauxs1 = 1.0/(0.0000719*(Ith(y,26)+30.0)/(1.0-exp(-0.148*(Ith(y,26)+30.0)))+0.000131*(Ith(y,26)+30.0)/(exp(0.0687*(Ith(y,26)+30.0))-1.0));
	}

	tauxs2 = 4.0*tauxs1;
	gksx = 1.0+0.8/(1.0+pow(0.5/Ith(y,3), 3.0));
	eks = 1.0/FonRT*log((K_o+prNaK*Na_o)/(Ith(y,27)+prNaK*Ith(y,25)));
	xiks = ScaleFactorGKs*gks*gksx*Ith(y,14)*Ith(y,15)*(Ith(y,26)-eks);
	Ith(y,30) = xiks;
	Ith(ydot,14) = (xs1ss-Ith(y,14))/tauxs1;
	Ith(ydot,15) = (xs2ss-Ith(y,15))/tauxs2;

	if (fabs(Ith(y,26)+47.13) > 0.001)
	{
		am = 0.32*1.0*(Ith(y,26)+47.13)/(1.0-exp(-0.1*(Ith(y,26)+47.13)));
	}
	else
	{
		am = 3.2;
	}

	bm = 0.08*exp(-Ith(y,26)/11.0);

	if (Ith(y,26) < -40.0)
	{
		ah = 0.135*exp((80.0+Ith(y,26))/-6.8);
		bh = 3.56*exp(0.079*Ith(y,26))+310000.0*exp(0.35*Ith(y,26));
		aj = (-127140.0*exp(0.2444*Ith(y,26))-0.00003474*exp(-0.04391*Ith(y,26)))*1.0*(Ith(y,26)+37.78)/(1.0+exp(0.311*(Ith(y,26)+79.23)));
		bj = 0.1212*exp(-0.01052*Ith(y,26))/(1.0+exp(-0.1378*(Ith(y,26)+40.14)));
	}
	else
	{
		ah = 0.0;
		bh = 1.0/(0.13*(1.0+exp((Ith(y,26)+10.66)/-11.1)));
		aj = 0.0;
		bj = 0.3*exp(-0.0000002535*Ith(y,26))/(1.0+exp(-0.1*(Ith(y,26)+32.0)));
	}

	Ith(ydot,16) = ah*(1.0-Ith(y,16))-bh*Ith(y,16);
	Ith(ydot,17) = aj*(1.0-Ith(y,17))-bj*Ith(y,17);
	Ith(ydot,18) = am*(1.0-Ith(y,18))-bm*Ith(y,18);
	ena = 1.0/FonRT*log(Na_o/Ith(y,25));
	xina = gna*Ith(y,16)*Ith(y,17)*Ith(y,18)*Ith(y,18)*Ith(y,18)*(Ith(y,26)-ena);
	xiNaCa = wca*jNaCa;
	fNaK = 1.0/(1.0+0.1245*exp(-0.1*Ith(y,26)*FonRT)+0.0365*sigma*exp(-Ith(y,26)*FonRT));
	xiNaK = ScaleFactorGNaK*gNaK*fNaK*Ith(y,25)/(Ith(y,25)+xkmnai)*K_o/(K_o+xkmko);
	Ith(y,32) = xiNaK;
	xika = fkatp*gkatp*akik*bkik*(Ith(y,26)-ek);
	Ith(ydot,19) = (Ith(y,1)-Ith(y,19))/taua;
	sparkV = exp(-ay*(Ith(y,26)+30.0))/(1.0+exp(-ay*(Ith(y,26)+30.0)));
	spark_rate = gryr/1.0*po*fabs(rxa)*sparkV;
	Ith(ydot,20) = spark_rate*Qr-Ith(y,20)*(1.0-taur*dCa_JSR/Ith(y,1))/taur;
	rt1 = -(Ith(y,26)+3.0)/15.0;
	rt2 = (Ith(y,26)+33.5)/10.0;
	rt3 = (Ith(y,26)+60.0)/10.0;
	rt4 = -Ith(y,26)/30.0*Ith(y,26)/30.0;
	rt5 = (Ith(y,26)+33.5)/10.0;
	xtos_inf = 1.0/(1.0+exp(rt1));
	ytos_inf = 1.0/(1.0+exp(rt2));
	xtof_inf = xtos_inf;
	ytof_inf = ytos_inf;
	rs_inf = 1.0/(1.0+exp(rt2));
	txs = 9.0/(1.0+exp(-rt1))+0.5;
	tys = 3000.0/(1.0+exp(rt3))+30.0;
	txf = 3.5*exp(rt4)+1.5;
	tyf = 20.0/(1.0+exp(rt5))+20.0;
	Ith(ydot,22) = (xtos_inf-Ith(y,22))/txs;
	Ith(ydot,24) = (ytos_inf-Ith(y,24))/tys;
	Ith(ydot,21) = (xtof_inf-Ith(y,21))/txf;
	Ith(ydot,23) = (ytof_inf-Ith(y,23))/tyf;
	xitos = gtos*Ith(y,22)*(Ith(y,24)+0.5*rs_inf)*(Ith(y,26)-ek);
	xitof = gtof*Ith(y,21)*Ith(y,23)*(Ith(y,26)-ek);
	xito = ScaleFactorGto*gto*(xitos+xitof);
	Ith(y,33) = xito;
	Ith(ydot,25) = -(xina+3.0*xiNaK+3.0*xiNaCa)/(wca*1000.0);
	past = floor(t/stim_period)*stim_period;

	if ((t-past >= stim_offset) && (t-past <= stim_offset+stim_duration))
	{
		i_Stim = stim_amplitude;
	}
	else
	{
		i_Stim = 0.0;
	}

	Itotal = -(xina+xik1+xikr+xiks+xito+xiNaCa+xica+xiNaK+i_Stim);
	Ith(ydot,26) = Itotal;
	Ith(ydot,27) = -(xikr+xiks+xik1+xito-xiNaK*2.0+i_Stim+xika)/(wca*1000.0);

   return(0);
}

static int check_flag(void *flagvalue, char *funcname, int opt)
{
  int *errflag;

  /* Check if SUNDIALS function returned NULL pointer - no memory allocated */
  if (opt == 0 && flagvalue == NULL) {
    fprintf(stderr, "\nSUNDIALS_ERROR: %s() failed - returned NULL pointer\n\n",
	    funcname);
    return(1); }

  /* Check if flag < 0 */
  else if (opt == 1) {
    errflag = (int *) flagvalue;
    if (*errflag < 0) {
      fprintf(stderr, "\nSUNDIALS_ERROR: %s() failed with flag = %d\n\n",
	      funcname, *errflag);
      return(1); }}

  /* Check if function returned NULL pointer - no memory allocated */
  else if (opt == 2 && flagvalue == NULL) {
    fprintf(stderr, "\nMEMORY_ERROR: %s() failed - returned NULL pointer\n\n",
	    funcname);
    return(1); }

  return(0);
}
