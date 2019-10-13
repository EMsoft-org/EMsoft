/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                     *
 * Copyright (c) 2019-2019, De Graef Group, Carnegie Mellon University *
 * All rights reserved.                                                *
 *                                                                     *
 * Author: William C. Lenthe                                           *
 *                                                                     *
 * This package is free software; you can redistribute it and/or       *
 * modify it under the terms of the GNU General Public License as      *
 * published by the Free Software Foundation; either version 2 of the  *
 * License, or (at your option) any later version.                     *
 *                                                                     *
 * This program is distributed in the hope that it will be useful,     *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of      *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *
 * GNU General Public License for more details.                        *
 *                                                                     *
 * You should have received a copy of the GNU General Public License   *
 * along with this program; if not, check the Free Software Foundation *
 * website: <https://www.gnu.org/licenses/old-licenses/gpl-2.0.html>   *
 *                                                                     *
 *                                                                     *
 * Interested in a commercial license? Contact:                        *
 *                                                                     *
 * Center for Technology Transfer and Enterprise Creation              *
 * 4615 Forbes Avenue, Suite 302                                       *
 * Pittsburgh, PA 15213                                                *
 *                                                                     *
 * phone. : 412.268.7393                                               *
 * email  : innovation@cmu.edu                                         *
 * website: https://www.cmu.edu/cttec/                                 *
 *                                                                     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

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
  int writeSHTfile_(char * fn, char const * nt, char const * doi,
                  int32_t * sgN, int32_t * sgS, int32_t * nAt, int32_t * aTy, float * aCd, float * lat,
                  float * fprm, int32_t * iprm,
                  int32_t * bw,double * alm) {
	  return sht::File::EMsoftEBSDRet(fn, nt, doi, *sgN, *sgS, *nAt, aTy, aCd, lat, fprm, iprm, *bw, alm);
  }
}
