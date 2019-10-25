//  ###################################################################
//  Copyright (c) 2013-2019, Marc De Graef Research Group/Carnegie Mellon University
//  All rights reserved.
// 
//  Redistribution and use in source and binary forms, with or without modification, are 
//  permitted provided that the following conditions are met:
// 
//      - Redistributions of source code must retain the above copyright notice, this list 
//         of conditions and the following disclaimer.
//      - Redistributions in binary form must reproduce the above copyright notice, this 
//         list of conditions and the following disclaimer in the documentation and/or 
//         other materials provided with the distribution.
//      - Neither the names of Marc De Graef, Carnegie Mellon University nor the names 
//         of its contributors may be used to endorse or promote products derived from 
//         this software without specific prior written permission.
// 
//  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
//  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
//  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
//  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
//  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
//  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
//  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
//  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
//  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
//  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//  ###################################################################
// --------------------------------------------------------------------------
//  EMsoft:sht_file.cpp
// --------------------------------------------------------------------------
// 
//  sht_file.cpp
// 
// > @author Marc De Graef, Carnegie Mellon University
// 
// > @brief sht_file.cpp is a wrapper to write .sht files from a fortran-90 program
// 
// > @date  10/25/19  WCL 1.0 original
// --------------------------------------------------------------------------

#include "sht_file.hpp"

//@brief     : write a file using EMsoft style EBSD data
//@prief fn  : file name to write
//@param iprm: integer parameters {sgn, mod, bw, sgn, sgs, nat, nel, elm, nsx, npx, grd}
//               header data
//                     sgn - effective space group number of spherical function
//                     mod - modality (see enumeration, EBSD == 1)
//                     bw  - bandwidth
//               crystal data
//                     sgn - space group number
//                     sgs - space group setting
//                     nat - number of atoms
//               simulation data
//                     nel - number of electrons
//                     elm - electron multiplier
//                     nsx - numsx (monte carlo grid size)
//                     npx - npx (master pattern grid size)
//                     grd - lattitude grid type
//@param fprm: floating point parameters {keV, sig, tht, res, a, b, c, alp, bet, gam, sgs, sge, sst, omg, kev, emn, esz, dmx, dmn, thk, c1, c2, c3, ddd, dmi}
//               header data
//                     keV - beam energy in keV
//                     sig - primary tilt angle
//                     tht - secondary tilt angle
//                     res - reserved parameter
//               crystal data
//                     a   - lattice constant a in nm
//                     b   - lattice constant b in nm
//                     c   - lattice constant c in nm
//                     alp - lattice constant alpha in degrees
//                     bet - lattice constant beta  in degrees
//                     gam - lattice constant gamma in degrees
//               simulation data
//                     sgs - sigma start
//                     sge - sigma end
//                     sst - sigma step
//                     omg - omega
//                     kev - keV
//                     emn - eHistMin
//                     esz - eBinSize
//                     dmx - depthMax
//                     dmn - depthMin
//                     thk - thickness
//                     c1  - c1
//                     c2  - c2
//                     c3  - c3
//                     ddd - sigDbDiff
//                     dmi - dMin
//@param doi : file DOI string (null terminated)
//@param note: file level notes (null terminated)
//@param alm : actual harmonics (uncompressed format)
//@param aTy : atom types (nAt atomic numbers)
//@param aCd : atom coordinates (nAt * 5 floats {x, y, z, occupancy, Debye-Waller in nm^2})
//@param vers: EMsoft version string (8 characters, null termination not required)
//@param cprm: string parameters as one concatenated sequence will null seperators (+ final null terminator)
//             {frm, nam, syb, ref}
//             frm - formula string (null terimated)
//             nam - material phase/name string (null terminated)
//             syb - structure symbol string (null terminated)
//             ref - reference string (null terminated)
//             nte - note string (null terminated)
//@return    : error code (void return function throws instead)
int writeShtFile(char* fn, int32_t const * iprm, float const * fprm,
                 char const * doi, char const * note, double const * alm,
                 int32_t * aTy, float * aCd, char const * vers, char const * cprm) {
	try {
		sht::File file;
		file.initFileEMsoft(iprm, fprm, doi, note, alm);//initialize header + harmonics
		file.addDataEMsoft(iprm + 3, fprm + 4, aTy, aCd, vers, cprm);//add crystal + simulation data
		// file.addDataEMsoft(iprm, fprm, aTy, aCd, vers, cprm);//add crystal + simulation data
		std::ofstream os(fn, std::ios::out | std::ios::binary);
		file.write(os);
	} catch (std::runtime_error& e) {
		std::cerr << e.what() << '\n';
		return 1;
	} catch (...) {
		std::cerr << "unknown error\n";
		return 2;
	}
	return 0;
}

extern "C" {
	//@brief     : write a file using EMsoft style EBSD data
	//@prief fn  : file name to write
	//@param iprm: integer parameters {sgn, mod, bw, sgn, sgs, nat, nel, elm, nsx, npx, grd}
	//               header data
	//                     sgn - effective space group number of spherical function
	//                     mod - modality (see enumeration, EBSD == 1)
	//                     bw  - bandwidth
	//               crystal data
	//                     sgn - space group number
	//                     sgs - space group setting
	//                     nat - number of atoms
	//               simulation data
	//                     nel - number of electrons
	//                     elm - electron multiplier
	//                     nsx - numsx (monte carlo grid size)
	//                     npx - npx (master pattern grid size)
	//                     grd - lattitude grid type
	//@param fprm: floating point parameters {keV, sig, tht, res, a, b, c, alp, bet, gam, sgs, sge, sst, omg, kev, emn, esz, dmx, dmn, thk, c1, c2, c3, ddd, dmi}
	//               header data
	//                     keV - beam energy in keV
	//                     sig - primary tilt angle
	//                     tht - secondary tilt angle
	//                     res - reserved parameter
	//               crystal data
	//                     a   - lattice constant a in nm
	//                     b   - lattice constant b in nm
	//                     c   - lattice constant c in nm
	//                     alp - lattice constant alpha in degrees
	//                     bet - lattice constant beta  in degrees
	//                     gam - lattice constant gamma in degrees
	//               simulation data
	//                     sgs - sigma start
	//                     sge - sigma end
	//                     sst - sigma step
	//                     omg - omega
	//                     kev - keV
	//                     emn - eHistMin
	//                     esz - eBinSize
	//                     dmx - depthMax
	//                     dmn - depthMin
	//                     thk - thickness
	//                     c1  - c1
	//                     c2  - c2
	//                     c3  - c3
	//                     ddd - sigDbDiff
	//                     dmi - dMin
	//@param doi : file DOI string (null terminated)
	//@param note: file level notes (null terminated)
	//@param alm : actual harmonics (uncompressed format)
	//@param aTy : atom types (nAt atomic numbers)
	//@param aCd : atom coordinates (nAt * 5 floats {x, y, z, occupancy, Debye-Waller in nm^2})
	//@param vers: EMsoft version string (8 characters, null termination not required)
	//@param cprm: string parameters as one concatenated sequence will null seperators (+ final null terminator)
	//             {frm, nam, syb, ref}
	//             frm - formula string (null terimated)
	//             nam - material phase/name string (null terminated)
	//             syb - structure symbol string (null terminated)
	//             ref - reference string (null terminated)
	//             nte - note string (null terminated)
	//@return    : error code (0 on success)
	int writeShtFile_(char* fn, int32_t const * iprm, float const * fprm,
	                 char const * doi, char const * note, double const * alm,
	                 int32_t * aTy, float * aCd, char const * vers, char const * cprm) {
	  return writeShtFile(fn, iprm, fprm,
	  	                  doi, note, alm,
	  	                  aTy, aCd, vers, cprm);
	}
}
