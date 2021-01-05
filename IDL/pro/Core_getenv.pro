;
; Copyright (c) 2013-2021, Marc De Graef Research Group/Carnegie Mellon University
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, are 
; permitted provided that the following conditions are met:
;
;     - Redistributions of source code must retain the above copyright notice, this list 
;        of conditions and the following disclaimer.
;     - Redistributions in binary form must reproduce the above copyright notice, this 
;        list of conditions and the following disclaimer in the documentation and/or 
;        other materials provided with the distribution.
;     - Neither the names of Marc De Graef, Carnegie Mellon University nor the names 
;        of its contributors may be used to endorse or promote products derived from 
;        this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
; ###################################################################
;--------------------------------------------------------------------------
; EMsoft:Core_getenv.pro
;--------------------------------------------------------------------------
;
; PROGRAM: Core_getenv.pro
;
;> @author Marc De Graef, Carnegie Mellon University
;
;> @brief read the environment variable for the EMsoft path from a json file
;
;> @date 10/08/15 MDG 1.0 initial implementation 
;> @date 11/11/15 MDG 1.1 added functionality for release version library location
;> @date 01/25/16 MDG 1.2 added new keywords
;--------------------------------------------------------------------------
function Core_getenv,data=data,bin=bin,lib=lib

common getenv_common, librarylocation

; default value for z
z = '---->  no keyword set for Core_getenv call'

; first parse the json configuration file
result = json_parse('~/.config/EMsoft/EMsoftConfig.json',/toarray)

; then extract the requested variable, depending on the keyword
if keyword_set(data) then z = result['EMdatapathname']
if keyword_set(bin) then begin
  z = result['EMsoftpathname']
; test to see whether or not this is a Release
  r = result['Release']
  if (r eq 'Yes') then begin
    z = z+'bin/'
  end else begin
    z = z+'Build/Bin/'
  endelse
endif
if keyword_set(lib) then z = result['EMsoftLibraryLocation']

return,z
end
