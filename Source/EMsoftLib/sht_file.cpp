/* 
* Copyright (c) 2018-2019, Marc De Graef Research Group/Carnegie Mellon University  *
* All rights reserved.                                                              *
*                                                                                   *
* Redistribution and use in source and binary forms, with or without                *
* modification, are permitted provided that the following conditions are met:       *
*                                                                                   *
*     - Redistributions of source code must retain the above copyright notice, this *
*       list of conditions and the following disclaimer.                            *
*     - Redistributions in binary form must reproduce the above copyright notice,   *
*       this list of conditions and the following disclaimer in the documentation   *
*       and/or other materials provided with the distribution.                      *
*     - Neither the copyright holder nor the names of its                           *
*       contributors may be used to endorse or promote products derived from        *
*       this software without specific prior written permission.                    *
*                                                                                   *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"       *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE         *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE    *
* DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE      *
* FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL        *
* DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR        *
* SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *
* CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,     *
* OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE         *
* USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.          *
*                                                                                   *
*/

#include "sht_file.hpp"

extern "C" {
  //@brief    : write a file using EMsoft style EBSD data
  //@prief fn : file name to write
  //@prief nt : notes string
  //@param sgN: space group number [1,230]
  //@param sgS: space group setting [1,2]
  //@param nAt: number of atoms
  //@param aTy: atom types (nAt atomic numbers)
  //@param aCd: atom coordinates, (nAt * 5 floats {x, y, z, occupancy, Debye-Waller in nm^2})
  //@param lat: lattice parameters {a, b, a, alpha, beta, gamma} (in nm / degree)
  //@param fprm: floating point parameters (float32 EMsoftED parameters in order)
  //@param iprm: integer parameters {# electrons, electron multiplier, numsx, npx, latgridtype}
  //@param bw : bandwidth
  //@param alm: actual harmonics (uncompressed format)
  int writeSHTfile_(char* fn, const char* nt, const char* doi,
                  int32_t* sgN, int32_t* sgS, int32_t* nAt, int32_t* aTy, float* aCd, float* lat,
                  float* fprm, int32_t* iprm,
                  int32_t* bw,double* alm) {
	  return sht::File::EMsoftEBSDRet(fn, nt, doi, *sgN, *sgS, *nAt, aTy, aCd, lat, fprm, iprm, *bw, alm);
  }
}
