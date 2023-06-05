'''
Created on Mar 3, 2014

@author: pascale
'''
import rmodel
import unittest
from test import rttovgui_unittest_class
import logging


class Test(rttovgui_unittest_class.RttovGuiUnitTest):

    def setUp(self):
        level_logging = logging.DEBUG
        self.p = rmodel.project.Project()

        logging.basicConfig(
            filename=(self.p.config.ENV['GUI_WRK_DIR'] +
                      "/rttovgui_test_atlas.log"),
            format=("[%(asctime)s] %(levelname)s [%(module)s:%(funcName)s"
                    ":%(lineno)d] %(message)s"),
            level=level_logging,
            datefmt="%Y:%m:%d %H:%M:%S",
            filemode="w")
        logging.info("start test atlas")

    def ctrl(self, p):
        import numpy
        print((p.myProfile['LATITUDE'], p.myProfile['LONGITUDE'],
               p.myProfile["SKIN"]["SURFTYPE"]))
        print("firstTimeSurface", p.firstTimeSurface)
        print("use atlas", p.useAtlas)
        print("###########call controleSurface !!!###########")
        p.controleSurface()
        print("######################")
        print(p.myEmissivity["EMIS_IN"])
        print(p.myCoeffs.fileName["standard"])
        print(type(p.myEmissivity["EMIS_IN"]))
        self.assertTrue(numpy.all(p.myEmissivity["EMIS_IN"] <= 1))
        self.assertTrue(numpy.all(p.myEmissivity["EMIS_IN"] >= 0))

    def test_atlas_landsat(self):
        print(">>>>>>>>>>>>>>>>>>>>>>>>>test atlas landsat")
        p = rmodel.project.Project()
        profileName = p.config.ENV[
            "RTTOV_GUI_PROFILE_DIR"] + "/cldaer101lev_allgas.H5"
        print(profileName)
        err = p.openProfile(profileName)
        self.assertEqual(err, 0)
        print("---------------------TEST ATLAS ---------------------------")
        coefFile = p.config.ENV['RTTOV_GUI_COEFF_DIR'] + \
            "/rttov13pred54L/rtcoef_landsat_4_tm_o3co2.dat"

        p.myCoeffs.fileName["standard"] = coefFile

        print(p.myCoeffs.fileName)

        err = p.loadCoefficients()
        self.assertEqual(err, 0)

        p.myProfile['LATITUDE'] = 45
        p.myProfile['LONGITUDE'] = 10
        p.myProfile["SKIN"]["SURFTYPE"] = 0

        err = p.openAtlas()
        self.assertEqual(err, 0)
        print("****************")
        print(type(p.myEmissivity["EMIS_IN"]))
        print("*************")
        import numpy
        self.assertTrue(numpy.any(p.myEmissivity["EMIS_IN"] > 0))
        self.ctrl(p)

    def test_atlas_1(self):
        print(">>>>>>>>>>>>>>>>>>>>>>>>>test atlas 1")
        p = rmodel.project.Project()
        profileName = p.config.ENV[
            "RTTOV_GUI_PROFILE_DIR"] + "/cldaer101lev_allgas.H5"
        print(profileName)
        err = p.openProfile(profileName)
        self.assertEqual(err, 0)
        print("---------------------TEST ATLAS 1-------------------------")
        coefFile = p.config.ENV['RTTOV_GUI_COEFF_DIR'] + \
            "/rttov13pred54L/rtcoef_eos_2_modis_o3co2.dat"

        p.myCoeffs.fileName["standard"] = coefFile

        print(p.myCoeffs.fileName)

        err = p.loadCoefficients()
        self.assertEqual(err, 0)

        p.myProfile['LATITUDE'] = 45
        p.myProfile['LONGITUDE'] = 10
        p.myProfile["SKIN"]["SURFTYPE"] = 0

        err = p.openAtlas()
        self.assertEqual(err, 0)
        self.ctrl(p)
        import numpy
        self.assertTrue(numpy.any(p.myEmissivity["EMIS_IN"] > 0))

    def test_atlas_2(self):
        print(">>>>>>>>>>>>>test atlas 2")
        print("test_atlas_2 surftype=1")

        p = rmodel.project.Project()
        profileName = p.config.ENV["RTTOV_GUI_PROFILE_DIR"] + \
            "/cldaer101lev_allgas.H5"

        print(profileName)
        err = p.openProfile(profileName)
        self.assertEqual(err, 0)
        print("---------------------TEST ATLAS 2 --------------------------")
        coefFile = p.config.ENV['RTTOV_GUI_COEFF_DIR'] + \
            "/rttov13pred54L/rtcoef_eos_2_modis_o3co2.dat"

        p.myCoeffs.fileName["standard"] = coefFile

        print(p.myCoeffs.fileName)

        err = p.loadCoefficients()
        self.assertEqual(err, 0)

        p.myProfile['LATITUDE'] = 45
        p.myProfile['LONGITUDE'] = 10
        p.myProfile["SKIN"]["SURFTYPE"] = 1
        print((p.myProfile['LATITUDE'], p.myProfile['LONGITUDE'],
               p.myProfile["SKIN"]["SURFTYPE"]))

        err = p.openAtlas()
        self.assertEqual(err, 0)
        self.assertFalse(p.firstTimeSurface)
        self.ctrl(p)

    def test_atlas_3(self):
        print(">>>>>>>>>test_atlas_3")
        p = rmodel.project.Project()
        profileName = p.config.ENV["RTTOV_GUI_PROFILE_DIR"] + \
            "/cldaer101lev_allgas.H5"
        print(profileName)
        err = p.openProfile(profileName)
        self.assertEqual(err, 0)
        print("---------------------TEST ATLAS ---------------------------")
        coefFile = p.config.ENV['RTTOV_GUI_COEFF_DIR'] + \
            "/rttov13pred54L/rtcoef_eos_2_modis_o3co2.dat"

        p.myCoeffs.fileName["standard"] = coefFile

        print(p.myCoeffs.fileName)

        err = p.loadCoefficients()
        self.assertEqual(err, 0)

        p.myProfile['LATITUDE'] = 45
        p.myProfile['LONGITUDE'] = 10
        p.myProfile["SKIN"]["SURFTYPE"] = 1

        err = p.openAtlas()
        self.assertEqual(err, 0)
        self.assertFalse(p.firstTimeSurface)
        self.ctrl(p)

    def test_atlas_4(self):
        print(">>>>>>>>>test_atlas_4")
        p = rmodel.project.Project()
        profileName = p.config.ENV[
            "RTTOV_GUI_PROFILE_DIR"] + "/cldaer101lev_allgas.H5"
        print(profileName)
        err = p.openProfile(profileName)
        self.assertEqual(err, 0)
        print("---------------------TEST ATLAS 4 ----------------------------")
        coefFile = p.config.ENV['RTTOV_GUI_COEFF_DIR'] + \
            "/rttov13pred54L/rtcoef_eos_2_modis_o3.dat"

        p.myCoeffs.fileName["standard"] = coefFile

        print(p.myCoeffs.fileName)

        err = p.loadCoefficients()
        self.assertEqual(err, 0)

        p.myProfile['LATITUDE'] = 45
        p.myProfile['LONGITUDE'] = 10
        p.myProfile["SKIN"]["SURFTYPE"] = 0

        err = p.openAtlas()
        self.assertEqual(err, 0)
        self.assertFalse(p.firstTimeSurface)
        self.ctrl(p)

    def test_atlas_5(self):
        print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>test_atlas_5")
        p = rmodel.project.Project()
        profileName = p.config.ENV[
            "RTTOV_GUI_PROFILE_DIR"] + "/cldaer101lev_allgas.H5"
        print(profileName)
        err = p.openProfile(profileName)
        self.assertEqual(err, 0)
        print("---------------------TEST ATLAS 5 ----------------------------")
        coefFile = p.config.ENV['RTTOV_GUI_COEFF_DIR'] + \
            "/rttov13pred54L/rtcoef_msg_1_seviri_o3.dat"

        p.myCoeffs.fileName["standard"] = coefFile

        print(p.myCoeffs.fileName)

        err = p.loadCoefficients()
        self.assertEqual(err, 0)

        p.myProfile['LATITUDE'] = 45
        p.myProfile['LONGITUDE'] = 10
        p.myProfile["SKIN"]["SURFTYPE"] = 1
        err = p.openAtlas()
        self.assertEqual(err, 0)

        err = p.dropCoefficients()
        self.assertEqual(err, 0)

        coefFile = p.config.ENV['RTTOV_GUI_COEFF_DIR'] + \
            "/rttov13pred54L/rtcoef_eos_1_modis_o3co2.dat"
        p.myCoeffs.fileName["standard"] = coefFile
        err = p.loadCoefficients()
        self.assertEqual(err, 0)
        err = p.openAtlas()
        self.assertEqual(err, 0)
        coefFile = p.config.ENV['RTTOV_GUI_COEFF_DIR'] + \
            "/rttov13pred101L/rtcoef_metop_2_iasi_7gas.H5"
        p.myCoeffs.fileName["standard"] = coefFile
        err = p.loadCoefficients()
        self.assertEqual(err, 0)
        print("has MW ?")
        self.assertFalse(p.myCoeffs.isMW())
        import rttov
        wavenumbers = rttov.getcoefval.rttov_get_coef_val_r1('WAVENUMBERS')
        print(min(wavenumbers), max(wavenumbers))
        self.assertEqual(min(wavenumbers), 645.0)
        self.assertEqual(max(wavenumbers), 2760.0)
        id_sensor = rttov.getcoefval.rttov_get_coef_val_i0('ID_SENSOR')
        print("id_sensor :", rttov.getcoefval.rttov_get_coef_val_i0(
            'ID_SENSOR'))
        self.assertEqual(id_sensor, 3)
        p.ctrlCoherence()
        self.check_option(p)
        err = p.runDirect()
        self.assertEqual(err, 0)

    def test_atlas_6(self):
        print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>test_atlas_6")
        p = rmodel.project.Project()
        profileName = p.config.ENV[
            "RTTOV_GUI_PROFILE_DIR"] + "/cldaer101lev_allgas.H5"
        print(profileName)
        err = p.openProfile(profileName)
        self.assertEqual(err, 0)
        print("---------------------TEST ATLAS 6----------------------------")
        coefFile = p.config.ENV['RTTOV_GUI_COEFF_DIR'] + \
            "/rttov13pred54L/rtcoef_msg_3_seviri_o3.dat"

        p.myCoeffs.fileName["standard"] = coefFile

        print(p.myCoeffs.fileName)

        err = p.loadCoefficients()
        self.assertEqual(err, 0)

        p.myProfile['LATITUDE'] = 45
        p.myProfile['LONGITUDE'] = 10
        p.myProfile["SKIN"]["SURFTYPE"] = 1
        print(p.myProfile['DATE'])
        err = p.openAtlas(3, 1)
        self.assertEqual(err, 0)
        print("ok test 6")

    def test_atlas_7(self):
        print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>test_atlas_7")
        p = rmodel.project.Project()
        profileName = p.config.ENV[
            "RTTOV_GUI_PROFILE_DIR"] + "/cldaer101lev_allgas.H5"
        print(profileName)
        err = p.openProfile(profileName)
        self.assertEqual(err, 0)
        print("---------------------TEST ATLAS 7----------------------------")
        coefFile = p.config.ENV['RTTOV_GUI_COEFF_DIR'] + \
            "/rttov13pred54L/rtcoef_metop_1_mhs.dat"

        p.myCoeffs.fileName["standard"] = coefFile

        print(p.myCoeffs.fileName)

        err = p.loadCoefficients()
        self.assertEqual(err, 0)

        p.myProfile['LATITUDE'] = 45
        p.myProfile['LONGITUDE'] = 10
        p.myProfile["SKIN"]["SURFTYPE"] = 1
        print(p.myProfile['DATE'])
        err = p.openAtlas(1, 1)
        self.assertEqual(err, 0)
        err = p.openAtlas(2, 1)
        self.assertEqual(err, 0)

    def test_emiss(self):
        print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>test_emis")
        p = rmodel.project.Project()
        profileName = p.config.ENV[
            "RTTOV_GUI_PROFILE_DIR"] + "/cldaer101lev_allgas.H5"
        print(profileName)
        err = p.openProfile(profileName)
        self.assertEqual(err, 0)
        print("---------------------TEST EMISS ----------------------------")
        coefFile = p.config.ENV['RTTOV_GUI_COEFF_DIR'] + \
            "/rttov13pred54L/rtcoef_msg_3_seviri_o3.dat"

        p.myCoeffs.fileName["standard"] = coefFile

        print(p.myCoeffs.fileName)

        err = p.loadCoefficients()
        self.assertEqual(err, 0)
        print("create surface")
        p.newSurface()
        p.openSurface(p.surfaceFileName)
        self.assertEqual(p.myEmissivity["EMIS_IN"][0], 0.)
        self.assertEqual(p.myEmissivity["CALCEMIS"][0], True)
        self.assertEqual(p.myEmissivity["EMIS_OUT"][0], 0.)
        self.assertEqual(p.myEmissivity["SPECULARITY"][0], 0.)
        p.fistTimeSurface = False
        # fix emissivity for channel 3 to 11
        p.myEmissivity["EMIS_IN"][:] = 1.
        p.myEmissivity["EMIS_OUT"][:] = 0.
        p.myEmissivity["CALCEMIS"][:] = False
        p.myEmissivity["EMIS_IN"][0:3] = 0.
        p.myEmissivity["EMIS_IN"][11] = 0.
        p.myEmissivity["CALCEMIS"][0:3] = True
        p.myEmissivity["CALCEMIS"][11] = True
        p.saveSurface(p.surfaceFileName)
        p.openSurface(p.surfaceFileName)
        print("before run file surface is :", p.surfaceFileName)
        self.assertEqual(p.myEmissivity["EMIS_IN"][3], 1.)
        self.assertEqual(p.myEmissivity["CALCEMIS"][3], False)
        p.runDirect()
        self.assertEqual(err, 0)
        p.openSurface(p.surfaceFileName)
        print("after run read file surface :", p.surfaceFileName)
        self.assertEqual(p.myEmissivity["EMIS_IN"][3], 1.)
        self.assertEqual(p.myEmissivity["CALCEMIS"][3], False)
        self.assertEqual(p.myEmissivity["EMIS_OUT"][3], 1.)
        print("TEST EMISS OK")

    def test_refl(self):
        print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>test_refl")
        p = rmodel.project.Project()
        profileName = p.config.ENV[
            "RTTOV_GUI_PROFILE_DIR"] + "/cldaer101lev_allgas.H5"
        print(profileName)
        err = p.openProfile(profileName)
        self.assertEqual(err, 0)
        print("---------------------TEST REFL ----------------------------")
        coefFile = p.config.ENV['RTTOV_GUI_COEFF_DIR'] + \
            "/rttov13pred54L/rtcoef_msg_3_seviri_o3.dat"

        p.myCoeffs.fileName["standard"] = coefFile

        print(p.myCoeffs.fileName)

        err = p.loadCoefficients()
        self.assertEqual(err, 0)
        print("create surface")
        p.newSurface()
        p.openSurface(p.surfaceFileName)
        self.assertEqual(p.myReflectance["REFL_IN"][0], 0.)
        self.assertEqual(p.myReflectance["CALCREFL"][0], True)
        self.assertEqual(p.myReflectance["REFL_OUT"][0], 0.)
        p.fistTimeSurface = False
        # fix reflextance for channel 1 to 3 and 12

        p.myReflectance["REFL_IN"][0:3] = 1.
        p.myReflectance["REFL_IN"][11] = 1.
        p.myReflectance["CALCREFL"][0:3] = False
        p.myReflectance["CALCREFL"][11] = False
        p.saveSurface(p.surfaceFileName)
        p.openSurface(p.surfaceFileName)
        print("before run file surface is :", p.surfaceFileName)
        self.assertEqual(p.myReflectance["REFL_IN"][0], 1.)
        self.assertEqual(p.myReflectance["CALCREFL"][0], False)
        p.myOption["ADDSOLAR"].value = True
        p.runDirect()
        self.assertEqual(err, 0)
        p.openSurface(p.surfaceFileName)
        print("after run read file surface :", p.surfaceFileName)
        self.assertEqual(p.myReflectance["REFL_IN"][0], 1.)
        self.assertEqual(p.myReflectance["REFL_OUT"][0], 1.)
        self.assertEqual(p.myReflectance["CALCREFL"][0], False)
        print("TEST REFL OK")


if __name__ == "__main__":
    unittest.main()
