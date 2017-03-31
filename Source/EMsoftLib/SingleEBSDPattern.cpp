

#include <iostream>
#include <stdlib.h>

#include "EMsoftLib.h"


int main(int argc, char const *argv[])
{

	std::cout << "EMsoftCgetEBSDPatterns Test" << std::endl;
	/*
! ipar(1) = detnumsx
! ipar(2) = detnumsy
! ipar(3) = detnumEbins
! ipar(4) = mcnsx
! ipar(5) = mpnpx
! ipar(6) = numset
! ipar(7) = numquats

! fpar(2) = enl%ypc
! fpar(3) = enl%delta
! fpar(4) = enl%MCsig
! fpar(5) = enl%omega
! fpar(6) = enl%thetac
! fpar(7) = enl%L
! fpar(8) = enl%beamcurrent
! fpar(9) = enl%dwelltime
*/

	size_t ipar[7] = {
		640,
		480,
		30,
		501,
		501,
		1,
		10
	 };

	 float fpar[9] = {
	0.0,
 	0.0,
 	10.0,
 	70.0,
 	0.0,
 	0.0,
 	20000.0,
 	1000.0,
 	1000.0
	};

	float* ebsdPattern = reinterpret_cast<float*>(malloc(ipar[0] * ipar[1] * ipar[6] * sizeof(float)));

	float* quats = reinterpret_cast<float*>(malloc(4 * ipar[6] * sizeof(float)));
	for(size_t i = 0; i < 4 * ipar[6]; i++)
	{
		quats[i*4 + 0] = 1.0;
		quats[i*4 + 1] = 0.0;
		quats[i*4 + 2] = 0.0;
		quats[i*4 + 3] = 0.0;

	}

	size_t aux = 2 * ipar[3];
	float* accum_e = reinterpret_cast<float*>(malloc(ipar[2] * aux * aux * sizeof(float)));
	for(size_t i = 0; i < ipar[2] * aux * aux ; i++)
	{
		accum_e[i] = 1.0f;
	}

	aux = 2 * ipar[4];
	float* mLPNH = reinterpret_cast<float*>(malloc(ipar[2] * ipar[5] * aux * aux * sizeof(float)));
	for(size_t i = 0; i < ipar[2] * ipar[5] * aux * aux ; i++)
	{
		mLPNH[i] = 1.0f;
	}

	float* mLPSH = reinterpret_cast<float*>(malloc(ipar[2] * ipar[5] * aux * aux * sizeof(float)));
	for(size_t i = 0; i < ipar[2] * ipar[5] * aux * aux; i++)
	{
		mLPSH[i] = 1.0f;
	}


	EMsoftCgetEBSDPatterns(ipar, fpar, ebsdPattern, quats, accum_e, mLPNH, mLPSH, NULL, 0, 0);

	std::cout << "EMsoftCgetEBSDPatterns Complete" << std::endl;


	return 0;
}
