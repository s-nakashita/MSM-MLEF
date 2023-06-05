#! /usr/bin/env python
# -*- coding: utf-8 -*-

import h5py as h5
import os


class MfasisLut(object):
    """ Class to keep ICE_SCHEME and CLW_SCHEME from a mfasis lut file"""

    def __init__(self, mfasis_file):
        self.ice_scheme = None
        self.clw_scheme = None
        try:
            print("mfasis_file=", mfasis_file)
            mfasis = h5.File(mfasis_file, 'r')
            self.ice_scheme = mfasis["MFASIS_CLD"]["ICE_SCHEME"][:][0]
            self.clw_scheme = mfasis["MFASIS_CLD"]["CLW_SCHEME"][:][0]
            mfasis.close()
        except Exception:
            print("Cannot read mfasis file", mfasis_file)


if __name__ == '__main__':
    rttov_coeff_dir = os.environ["RTTOV_GUI_COEFF_DIR"] + "/mfasis_lut/"
    mfasisFile = rttov_coeff_dir + "rttov_mfasis_cld_goes_16_abi_deff.H5"
    mfasisObject = MfasisLut(mfasisFile)
    print(mfasisObject.ice_scheme)
    print(mfasisObject.clw_scheme)
