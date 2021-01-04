"""
BSD 3-Clause License

Copyright (c) 2020, Chaoyi Zhu, Marc De Graef Research Group/Carnegie Mellon University
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"""
from setuptools import setup, find_packages, Extension
from setuptools.command.install import install
from codecs import open
from os import path
import sys

with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

setup(
    name="EMsoft",
    version="1.0",
    author="Chaoyi Zhu",
    author_email="zchaoyi@andrew.cmu.edu",
    description="Python wrapper for EMsoft",
    long_description=long_description,
    long_description_content_type="text/markdown",
    packages=['EMsoft'],
    package_data={'EMsoft': ['*.so','.f2py_f2cmap']},
    include_package_data=True,
    classifiers=[
        'Development Status :: 3 - Alpha',
        'Topic :: Scientific/Engineering :: Physics',
        'License :: OSI Approved :: BSD License',
        'Programming Language :: Python :: 3.7',
    ],
    python_requires='>=3.7',
    install_requires=['numpy', 'h5py', 'matplotlib',
                      'scikit-image', 'pyyaml', 'notebook','ipywidgets'],
)
