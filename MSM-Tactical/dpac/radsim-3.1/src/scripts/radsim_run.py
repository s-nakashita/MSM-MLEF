#!/usr/bin/env python
#-------------------------------------------------------------------------------
# Synopsis:
#
# Usage info:
#
#   radsim_run.py -h
#
# Description:
#
#   Generate a RadSim configuration file and (optionally) run RadSim using the
#   generated file. All RadSim variables can be specified as arguments. Some
#   arguments are mandatory. Optional arguments take the RadSim defaults if
#   unspecified.
#
#-------------------------------------------------------------------------------

import argparse
import string
import os
import sys
import subprocess

# Valid values for RadSim options
VALID_MODEL_FILETYPE = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
VALID_OUTPUT_MODE = (1, 2, 3)

# Valid values for RTTOV options
VALID_RTTOV_COEFFS_TYPE = ('.dat', '.H5')
VALID_EMIS_ATLAS_ID = (1, 2, 3)
VALID_IRCLOUD_CLW_SCHEME = (1, 2)
VALID_IRCLOUD_ICE_SCHEME = (1, 2, 3)
VALID_IRCLOUD_ICEDE_PARAM = (1, 2, 3, 4)
VALID_IR_SCATT_MODEL = (1, 2)
VALID_VIS_SCATT_MODEL = (1, 2, 3)
VALID_FASTEM_VERSION = (0, 1, 2, 3, 4, 5, 6)
VALID_IR_SEA_EMIS_MODEL = (1, 2)
VALID_SOLAR_SEA_BRDF_MODEL = (1, 2)
VALID_INTERP_MODE = (1, 2, 3, 4, 5)
VALID_MW_CLW_SCHEME = (1, 2, 3)


def parse_args():
    """
    Parse the script arguments
    """

    class TFAction(argparse.Action):
        def __init__(self, option_strings, dest, nargs=None, **kwargs):
            super(TFAction, self).__init__(option_strings, dest, **kwargs)
        def __call__(self, parser, namespace, values, option_string=None):
            if values.lower() in ('t', 'true'):
                setattr(namespace, self.dest, True)
            elif values.lower() in ('f', 'false'):
                setattr(namespace, self.dest, False)
            else:
                raise ValueError("True/False arguments require T or F")

    parser = argparse.ArgumentParser( \
        description='Generate RadSim configuration file and optionally run RadSim', conflict_handler='resolve')

    # Script-specific
    parser.add_argument('--config_file', dest='config_file', type=str, required=True,
                        help='name of output RadSim config file', metavar='FILE')
    parser.add_argument('--write_config_only', dest='write_config_only', type=str, action=TFAction, required=False, default=False,
                        help='generate RadSim config file only, don''t run RadSim', metavar='T|F')
    parser.add_argument('--radsim_bin_dir', dest='radsim_bin_dir', type=str, required=False, default='bin',
                        help='path to RadSim bin/ directory', metavar='DIR')

    # RadSim input data
    parser.add_argument('--model_datafile', dest='model_datafile', type=str, required=True,
                        help='name of input model data file', metavar='FILE')
    parser.add_argument('--model_ancil_datafile', dest='model_ancil_datafile', type=str, required=False,
                        help='name of ancillary input model data file', metavar='FILE')
    parser.add_argument('--model_ancil2_datafile', dest='model_ancil2_datafile', type=str, required=False,
                        help='name of second ancillary input model data file', metavar='FILE')
    parser.add_argument('--model_filetype', dest='model_filetype', type=int, required=True, choices=VALID_MODEL_FILETYPE,
                        help='type of model data (0=UM PP, 1=ECMWF GRIB, 2/3/4=NWPSAF profiles (60/91/137L), 5=ECMWF NETCDF, 6=ICON GRIB, 7=HARMONIE GRIB)',
                        metavar='|'.join(map(str, VALID_MODEL_FILETYPE)))
    parser.add_argument('--rttov_coeffs_type', dest='rttov_coeffs_type', type=str, required=False, choices=VALID_RTTOV_COEFFS_TYPE,
                        help='suffix for RTTOV coefficient files', metavar='|'.join(VALID_RTTOV_COEFFS_TYPE))
    parser.add_argument('--rttov_coeffs_options', dest='rttov_coeffs_options', type=str, required=False,
                        help='additional string in RTTOV rtcoef file name', metavar='STRING')
    parser.add_argument('--rttov_coeffs_dir', dest='rttov_coeffs_dir', type=str, required=False,
                        help='path to RTTOV rtcoef coefficients directory', metavar='DIR')
    parser.add_argument('--rttov_scaer_dir', dest='rttov_scaer_dir', type=str, required=False,
                        help='path to RTTOV scaer coefficients directory', metavar='DIR')
    parser.add_argument('--rttov_sccld_dir', dest='rttov_sccld_dir', type=str, required=False,
                        help='path to RTTOV sccld coefficients directory', metavar='DIR')
    parser.add_argument('--rttov_mfasis_cld_dir', dest='rttov_mfasis_cld_dir', type=str, required=False,
                        help='path to RTTOV MFASIS cloud LUT directory', metavar='DIR')
    parser.add_argument('--rttov_hydrotable_dir', dest='rttov_hydrotable_dir', type=str, required=False,
                        help='path to RTTOV hydrotable coefficients directory', metavar='DIR')
    parser.add_argument('--rttov_pccoeffs_dir', dest='rttov_pccoeffs_dir', type=str, required=False,
                        help='path to RTTOV pccoef coefficients directory', metavar='DIR')
    parser.add_argument('--rttov_htfrtc_coeffs_dir', dest='rttov_htfrtc_coeffs_dir', type=str, required=False,
                        help='path to RTTOV HTFRTC coefficients directory', metavar='DIR')
    parser.add_argument('--rttov_coeff_file', dest='rttov_coeff_file', type=str, required=False,
                        help='RTTOV rtcoef coefficients filename', metavar='FILE')
    parser.add_argument('--rttov_scaer_file', dest='rttov_scaer_file', type=str, required=False,
                        help='RTTOV scaer coefficients filename', metavar='FILE')
    parser.add_argument('--rttov_sccld_file', dest='rttov_sccld_file', type=str, required=False,
                        help='RTTOV sccld coefficients filename', metavar='FILE')
    parser.add_argument('--rttov_mfasis_cld_file', dest='rttov_mfasis_cld_file', type=str, required=False,
                        help='RTTOV MFASIS cloud LUT filename', metavar='FILE')
    parser.add_argument('--rttov_hydrotable_file', dest='rttov_hydrotable_file', type=str, required=False,
                        help='RTTOV hydrotable coefficients filename', metavar='FILE')
    parser.add_argument('--rttov_pccoeff_file', dest='rttov_pccoeff_file', type=str, required=False,
                        help='RTTOV pccoef coefficients filename', metavar='FILE')
    parser.add_argument('--rttov_htfrtc_static_file', dest='rttov_htfrtc_static_file', type=str, required=False,
                        help='RTTOV HTFRTC static coefficients filename', metavar='FILE')
    parser.add_argument('--rttov_htfrtc_sensor_file', dest='rttov_htfrtc_sensor_file', type=str, required=False,
                        help='RTTOV HTFRTC sensor coefficients filename', metavar='FILE')
    parser.add_argument('--obs_datafile', dest='obs_datafile', type=str, required=False,
                        help='name of input observations data file', metavar='FILE')
    parser.add_argument('--read_footprint_file', dest='read_footprint_file', type=str, required=False,
                        help='path of existing netCDF file containing obs-footprint mapping data', metavar='FILE')
    parser.add_argument('--write_footprint_file', dest='write_footprint_file', type=str, required=False,
                        help='name of netCDF file to which obs-footprint mapping data will be written', metavar='FILE')
    parser.add_argument('--emiss_atlas_dir', dest='emiss_atlas_dir', type=str, required=False,
                        help='directory containing RTTOV emissivity atlases', metavar='DIR')
    parser.add_argument('--brdf_atlas_dir', dest='brdf_atlas_dir', type=str, required=False,
                        help='directory containing RTTOV BRDF atlas', metavar='DIR')

    # Satellite instrument options
    parser.add_argument('--platform', dest='platform', type=str, required=True,
                        help='RTTOV satellite platform name', metavar='STRING')
    parser.add_argument('--satid', dest='satid', type=int, required=True,
                        help='RTTOV satellite platform number', metavar='INTEGER')
    parser.add_argument('--inst', dest='inst', type=str, required=True,
                        help='RTTOV instrument name', metavar='STRING')
    parser.add_argument('--channels', dest='channels', type=int, required=False, default=[0], nargs='+',
                        help='channel list (omit or 0 for all channels)', metavar='INTEGER')

    # RadSim run-time options
    parser.add_argument('--temporal_data', dest='temporal_data', type=str, action=TFAction, required=False,
                        help='enable/disable temporal interpolation', metavar='T|F')
    parser.add_argument('--enable_footprints', dest='enable_footprints', type=str, action=TFAction, required=False,
                        help='enable/disable footprint simulation', metavar='T|F')
    parser.add_argument('--use_emiss_atlas', dest='use_emiss_atlas', type=str, action=TFAction, required=False,
                        help='enable/disable use of RTTOV emissivity atlases', metavar='T|F')
    parser.add_argument('--emis_atlas_id', dest='emis_atlas_id', type=int, required=False, choices=VALID_EMIS_ATLAS_ID,
                        help='emissivity atlas ID', metavar='|'.join(map(str, VALID_EMIS_ATLAS_ID)))
    parser.add_argument('--use_brdf_atlas', dest='use_brdf_atlas', type=str, action=TFAction, required=False,
                        help='enable/disable use of RTTOV BRDF atlas', metavar='T|F')
    parser.add_argument('--use_all_atlas_months', dest='use_all_atlas_months', type=str, action=TFAction, required=False,
                        help='for NWP SAF profile datasets use atlas data for month of each profile', metavar='T|F')
    parser.add_argument('--run_scatt', dest='run_scatt', type=str, action=TFAction, required=False,
                        help='enable/disable MW cloud simulations', metavar='T|F')
    parser.add_argument('--run_mfasis', dest='run_mfasis', type=str, action=TFAction, required=False,
                        help='enable/disable MFASIS visible cloudy simulations', metavar='T|F')
    parser.add_argument('--ircloud_use_model_clw_deff', dest='ircloud_use_model_clw_deff', 
                        type=str, action=TFAction, required=False,
                        help='enable/disable use of ICON cloud liquid effective diameter fields if present', metavar='T|F')
    parser.add_argument('--ircloud_use_model_ice_deff', dest='ircloud_use_model_ice_deff', 
                        type=str, action=TFAction, required=False,
                        help='enable/disable use of ICON cloud ice effective diameter fields if present', metavar='T|F')
    parser.add_argument('--calc_geo_sat_angles', dest='calc_geo_sat_angles', type=str, action=TFAction, required=False,
                        help='compute satellite zenith and azimuth angles for geostationary sensors', metavar='T|F')
    parser.add_argument('--geo_sat_lat', dest='geo_sat_lat', type=float, required=False,
                        help='geostationary satellite latitude (degrees)', metavar='FLOAT')
    parser.add_argument('--geo_sat_lon', dest='geo_sat_lon', type=float, required=False,
                        help='geostationary satellite longitude (degrees)', metavar='FLOAT')
    parser.add_argument('--geo_sat_height', dest='geo_sat_height', type=float, required=False,
                        help='geostationary satellite altitude (km)', metavar='FLOAT')
    parser.add_argument('--zen_angle', dest='zen_angle', type=float, required=False,
                        help='satellite zenith angle (degrees)', metavar='FLOAT')
    parser.add_argument('--azi_angle', dest='azi_angle', type=float, required=False,
                        help='satellite azimuth angle (degrees)', metavar='FLOAT')
    parser.add_argument('--sun_zen_angle', dest='sun_zen_angle', type=float, required=False,
                        help='solar zenith angle (degrees)', metavar='FLOAT')
    parser.add_argument('--sun_azi_angle', dest='sun_azi_angle', type=float, required=False,
                        help='solar azimuth angle (degrees)', metavar='FLOAT')
    parser.add_argument('--co2_max_ppmv', dest='co2_max_ppmv', type=float, required=False,
                        help='use background CO2 profile with this maximum value (ppmv over dry air)', metavar='FLOAT')
    parser.add_argument('--ssu_co2_cell_pressure', dest='ssu_co2_cell_pressure', type=float, required=False, default=None, nargs=3,
                        help='CO2 cell pressures for SSU PMC shift coefficients', metavar='FLOAT')
    parser.add_argument('--cads_height_assign_threshold', dest='cads_height_assign_threshold', type=float, required=False,
                        help='set positive to output CADS height assignments (usual value is 0.01)', metavar='FLOAT')
    parser.add_argument('--default_brdf_land', dest='default_brdf_land', type=float, required=False,
                        help='specify default BRDF for land to use instead of RTTOV default', metavar='FLOAT')
    parser.add_argument('--default_brdf_seaice', dest='default_brdf_seaice', type=float, required=False,
                        help='specify default BRDF for seaice to use instead of RTTOV default', metavar='FLOAT')
    parser.add_argument('--seaice_threshold', dest='seaice_threshold', type=float, required=False,
                        help='seaice threshold value', metavar='FLOAT')
    parser.add_argument('--coast_threshold', dest='coast_threshold', type=float, required=False,
                        help='coast threshold value', metavar='FLOAT')
    parser.add_argument('--output_mode', dest='output_mode', type=int, required=False, choices=VALID_OUTPUT_MODE,
                        help='diagnostic output level (1=normal, 2=verbose, 3=debug)', metavar='|'.join(map(str, VALID_OUTPUT_MODE)))
    parser.add_argument('--max_array_size', dest='max_array_size', type=int, required=False,
                        help='maximum size of output array (MB)', metavar='INTEGER')
    parser.add_argument('--max_profs', dest='max_profs', type=int, required=False,
                        help='maximum number of profiles passed to RTTOV per call', metavar='INTEGER')
    parser.add_argument('--nthreads', dest='nthreads', type=int, required=False,
                        help='number of threads to use (RTTOV must be compiled with OpenMP)', metavar='INTEGER')
    parser.add_argument('--nprofs_per_call', dest='nprofs_per_call', type=int, required=False,
                        help='number of profiles passed to RTTOV per call', metavar='INTEGER')

    # RTTOV options
    parser.add_argument('--addpc', dest='addpc', type=str, action=TFAction, required=False,
                        help='enable/disable PC-RTTOV simulations', metavar='T|F')
    parser.add_argument('--htfrtc', dest='htfrtc', type=str, action=TFAction, required=False,
                        help='enable/disable HTFRTC simulations', metavar='T|F')
    parser.add_argument('--npcscores', dest='npcscores', type=int, required=False,
                        help='number of PC scores to compute for PC-RTTOV or HTFRTC', metavar='INTEGER')
    parser.add_argument('--ipcreg', dest='ipcreg', type=int, required=False,
                        help='PC predictor set for PC-RTTOV', metavar='INTEGER')
    parser.add_argument('--ipcbnd', dest='ipcbnd', type=int, required=False,
                        help='PC spectral band for PC-RTTOV', metavar='INTEGER')
    parser.add_argument('--addsolar', dest='addsolar', type=str, action=TFAction, required=False,
                        help='enable/disable solar radiation', metavar='T|F')
    parser.add_argument('--rayleigh_single_scatt', dest='rayleigh_single_scatt', type=str, action=TFAction, required=False,
                        help='enable/disable Rayleigh single-scattering', metavar='T|F')
    parser.add_argument('--rayleigh_max_wavelength', dest='rayleigh_max_wavelength', type=float, required=False,
                        help='Maximum wavelength for Rayleigh scattering calculations', metavar='FLOAT')
    parser.add_argument('--rayleigh_min_pressure', dest='rayleigh_min_pressure', type=float, required=False,
                        help='Minumum pressure for Rayleigh scattering calculations', metavar='FLOAT')
    parser.add_argument('--dom_rayleigh', dest='dom_rayleigh', type=str, action=TFAction, required=False,
                        help='enable/disable Rayleigh multiple scattering in DOM solver', metavar='T|F')
    parser.add_argument('--ir_addaerosols_cams', dest='ir_addaerosols_cams', type=str, action=TFAction, required=False,
                        help='enable/disable VIS/IR CAMS aerosol simulations', metavar='T|F')
    parser.add_argument('--ir_addclouds', dest='ir_addclouds', type=str, action=TFAction, required=False,
                        help='enable/disable VIS/IR cloud simulations', metavar='T|F')
    parser.add_argument('--ir_scatt_model', dest='ir_scatt_model', type=int, required=False, choices=VALID_IR_SCATT_MODEL,
                        help='IR scattering model', metavar='|'.join(map(str, VALID_IR_SCATT_MODEL)))
    parser.add_argument('--vis_scatt_model', dest='vis_scatt_model', type=int, required=False, choices=VALID_VIS_SCATT_MODEL,
                        help='solar scattering model', metavar='|'.join(map(str, VALID_VIS_SCATT_MODEL)))
    parser.add_argument('--dom_nstreams', dest='dom_nstreams', type=int, required=False,
                        help='number of DOM streams', metavar='INTEGER')
    parser.add_argument('--dom_accuracy', dest='dom_accuracy', type=float, required=False,
                        help='DOM accuracy parameter', metavar='FLOAT')
    parser.add_argument('--dom_opdep_threshold', dest='dom_opdep_threshold', type=float, required=False,
                        help='DOM opdep threshold parameter', metavar='FLOAT')
    parser.add_argument('--ircloud_clw_scheme', dest='ircloud_clw_scheme', type=int, required=False, choices=VALID_IRCLOUD_CLW_SCHEME,
                        help='CLW cloud optical properties for VIS/IR cloud simulations', metavar='|'.join(map(str, VALID_IRCLOUD_CLW_SCHEME)))
    parser.add_argument('--ircloud_ice_scheme', dest='ircloud_ice_scheme', type=int, required=False, choices=VALID_IRCLOUD_ICE_SCHEME,
                        help='ice cloud optical properties for VIS/IR cloud simulations', metavar='|'.join(map(str, VALID_IRCLOUD_ICE_SCHEME)))
    parser.add_argument('--ircloud_icede_param', dest='ircloud_icede_param', type=int, required=False, choices=VALID_IRCLOUD_ICEDE_PARAM,
                        help='ice cloud effective diameter parameterisation for VIS/IR cloud simulations', metavar='|'.join(map(str, VALID_IRCLOUD_ICEDE_PARAM)))
    parser.add_argument('--cldcol_threshold', dest='cldcol_threshold', type=float, required=False,
                        help='cldcol_threshold value for VIS/IR scattering simulations', metavar='FLOAT')
    parser.add_argument('--fastem_version', dest='fastem_version', type=int, required=False, choices=VALID_FASTEM_VERSION,
                        help='FASTEM MW emissivity model version', metavar='|'.join(map(str, VALID_FASTEM_VERSION)))
    parser.add_argument('--ir_sea_emis_model', dest='ir_sea_emis_model', type=int, required=False, choices=VALID_IR_SEA_EMIS_MODEL,
                        help='IR sea emissivity model', metavar='|'.join(map(str, VALID_IR_SEA_EMIS_MODEL)))
    parser.add_argument('--solar_sea_brdf_model', dest='solar_sea_brdf_model', type=int, required=False, choices=VALID_SOLAR_SEA_BRDF_MODEL,
                        help='solar sea BRDF model', metavar='|'.join(map(str, VALID_SOLAR_SEA_BRDF_MODEL)))
    parser.add_argument('--ozone_data', dest='ozone_data', type=str, action=TFAction, required=False,
                        help='enable/disable ozone in VIS/IR simulations', metavar='T|F')
    parser.add_argument('--clw_data', dest='clw_data', type=str, action=TFAction, required=False,
                        help='enable/disable cloud liquid water in non-scattering MW simulations', metavar='T|F')
    parser.add_argument('--cc_threshold', dest='cc_threshold', type=float, required=False,
                        help='cc_threshold value for scattering MW simulations', metavar='FLOAT')
    parser.add_argument('--ice_polarisation', dest='ice_polarisation', type=float, required=False,
                        help='ice_polarisation value for scattering MW simulations', metavar='FLOAT')
    parser.add_argument('--apply_reg_limits', dest='apply_reg_limits', type=str, action=TFAction, required=False,
                        help='enable/disable apply_reg_limits option', metavar='T|F')
    parser.add_argument('--interp_mode', dest='interp_mode', type=int, required=False, choices=VALID_INTERP_MODE,
                        help='RTTOV interpolation mode', metavar='|'.join(map(str, VALID_INTERP_MODE)))
    parser.add_argument('--addrefrac', dest='addrefrac', type=str, action=TFAction, required=False,
                        help='enable/disable atmospheric refraction calculation', metavar='T|F')
    parser.add_argument('--plane_parallel', dest='plane_parallel', type=str, action=TFAction, required=False,
                        help='enable/disable strict plane-parallel geometry (no curvature or refraction)', metavar='T|F')
    parser.add_argument('--do_lambertian', dest='do_lambertian', type=str, action=TFAction, required=False,
                        help='enable/disable Lambertian surface option', metavar='T|F')
    parser.add_argument('--lambertian_fixed_angle', dest='lambertian_fixed_angle', type=str, action=TFAction, required=False,
                        help='select the Lambertian fixed angle/parameterised angle option', metavar='T|F')
    parser.add_argument('--do_nlte_correction', dest='do_nlte_correction', type=str, action=TFAction, required=False,
                        help='enable/disable NLTE correction (requires compatible RTTOV rtcoef file)', metavar='T|F')
    parser.add_argument('--mw_clw_scheme', dest='mw_clw_scheme', type=int, required=False, choices=VALID_MW_CLW_SCHEME,
                        help='non-scattering MW CLW scheme', metavar='|'.join(map(str, VALID_MW_CLW_SCHEME)))
    parser.add_argument('--mw_clw_cloud_top', dest='mw_clw_cloud_top', type=float, required=False,
                        help='cloud top (hPa) for non-scattering MW CLW calculations', metavar='FLOAT')

    # Output options
    parser.add_argument('--output_dir', dest='output_dir', type=str, required=False, default='',
                        help='directory for RadSim output', metavar='DIR')
    parser.add_argument('--output_file', dest='output_file', type=str, required=False, default='',
                        help='RadSim output file name', metavar='FILE')
    parser.add_argument('--write_profiles', dest='write_profiles', type=str, action=TFAction, required=False,
                        help='enable/disable output of profile data', metavar='T|F')
    parser.add_argument('--write_latlon', dest='write_latlon', type=str, action=TFAction, required=False,
                        help='enable/disable output of lat/lon data', metavar='T|F')
    parser.add_argument('--write_geom_height', dest='write_geom_height', type=str, action=TFAction, required=False,
                        help='enable/disable output of geometric height data', metavar='T|F')
    parser.add_argument('--write_trans', dest='write_trans', type=str, action=TFAction, required=False,
                        help='enable/disable output of calculated transmittances', metavar='T|F')
    parser.add_argument('--write_tjac', dest='write_tjac', type=str, action=TFAction, required=False,
                        help='enable/disable output of temperature Jacobians', metavar='T|F')
    parser.add_argument('--write_qjac', dest='write_qjac', type=str, action=TFAction, required=False,
                        help='enable/disable output of water vapour Jacobians', metavar='T|F')
    parser.add_argument('--write_o3jac', dest='write_o3jac', type=str, action=TFAction, required=False,
                        help='enable/disable output of ozone Jacobians', metavar='T|F')
    parser.add_argument('--write_tskinjac', dest='write_tskinjac', type=str, action=TFAction, required=False,
                        help='enable/disable output of Tskin Jacobians', metavar='T|F')
    parser.add_argument('--write_wind10mjac', dest='write_wind10mjac', type=str, action=TFAction, required=False,
                        help='enable/disable output of 10m wind u/v Jacobians', metavar='T|F')
    parser.add_argument('--write_emissjac', dest='write_emissjac', type=str, action=TFAction, required=False,
                        help='enable/disable output of surface emissivity Jacobians', metavar='T|F')
    parser.add_argument('--write_radiances', dest='write_radiances', type=str, action=TFAction, required=False,
                        help='enable/disable output of radiances instead of BTs and reflectances', metavar='T|F')
    parser.add_argument('--write_emiss', dest='write_emiss', type=str, action=TFAction, required=False,
                        help='enable/disable output of surface emissivities', metavar='T|F')
    parser.add_argument('--write_brdf', dest='write_brdf', type=str, action=TFAction, required=False,
                        help='enable/disable output of surface BRDFs', metavar='T|F')

    return parser.parse_args()


def check_config(args):
    """
    Carry out some checks on the supplied arguments
    """

    def guess_dir(attr_dir, sub_dir, flagname):
        """
        Check existence of an RTTOV coef subdirectory and attempt to guess
        from the rtcoef directory if not specified
          attr_dir: string, directory attribute of args to check
          sub_dir: string, name of subdirectory to use as a guess
          flagname: name of associated flag
        """
        if getattr(args, attr_dir) is None:
            if args.rttov_coeffs_dir is None:
                print('ERROR: attempt to guess ' + attr_dir + ' failed, ' + \
                      '--' + attr_dir + ' must be specified if ' + flagname + ' is true')
                return 1
            rtcoef_dir = args.rttov_coeffs_dir
            if rtcoef_dir[-1] == os.sep: rtcoef_dir = rtcoef_dir[:-1]
            new_dir = os.path.dirname(rtcoef_dir) + os.sep + sub_dir
            if not os.path.exists(new_dir):
                print('ERROR: attempt to guess ' + attr_dir + ' failed, ' + \
                      '--' + attr_dir + ' must be specified if ' + flagname + ' is true')
                return 1
            else:
                setattr(args, attr_dir, new_dir)
        elif not os.path.exists(getattr(args, attr_dir)):
            print('ERROR: invalid ' + attr_dir + ', this must be specified if ' + flagname + ' is true')
            return 1
        return 0

    # Check RadSim bin/ dir
    args.radsim_bin_dir = args.radsim_bin_dir.strip()
    if not os.path.exists(args.radsim_bin_dir):
        if os.path.exists('../bin'):
            args.radsim_bin_dir = '../bin'
        else:
            print('ERROR: invalid RadSim bin/ directory ' + args.radsim_bin_dir)
            return 1

    args.radsim_exe = os.sep.join((args.radsim_bin_dir, 'radsim.exe'))
    if not os.path.exists(args.radsim_exe):
        print('ERROR: radsim.exe executable not found ' + args.radsim_exe)
        return 1

    args.radsim_run = os.sep.join((args.radsim_bin_dir, 'radsim_run'))
    if not os.path.exists(args.radsim_run):
        print('ERROR: radsim_run script not found ' + args.radsim_run)
        return 1

    print('Found RadSim executable ' + args.radsim_exe + ' and script ' + args.radsim_run)

    # Ensure paths exist; make a guess for RTTOV cloud coef directories if not specified
    if args.obs_datafile is not None and not os.path.exists(args.obs_datafile):
        print('ERROR: specified obs_datafile does not exist ' + args.obs_datafile)
        return 1

    if args.read_footprint_file is not None and not os.path.exists(args.read_footprint_file):
        print('ERROR: specified read_footprint_file does not exist ' + args.read_footprint_file)
        return 1

    if not os.path.exists(args.model_datafile):
        print('ERROR: model_datafile does not exist ' + args.model_datafile)
        return 1

    if args.model_ancil_datafile is not None:
        if not os.path.exists(args.model_ancil_datafile):
            print('ERROR: model_ancil_datafile does not exist ' + args.model_ancil_datafile)
            return 1

    if args.use_emiss_atlas and not args.emiss_atlas_dir is None:
        if not os.path.exists(args.emiss_atlas_dir):
            print('ERROR: valid emiss_atlas_dir required if use_emiss_atlas is true ' + args.emiss_atlas_dir)
            return 1

    if args.use_brdf_atlas and not args.brdf_atlas_dir is None:
        if not os.path.exists(args.brdf_atlas_dir):
            print('ERROR: valid brdf_atlas_dir required if use_brdf_atlas is true ' + args.brdf_atlas_dir)
            return 1

    if args.ir_addaerosols_cams:
        if args.addsolar:
            status = guess_dir('rttov_scaer_dir', 'cldaer_visir', 'ir_addaerosols_cams')
            if status != 0: return status
        else:
            status = guess_dir('rttov_scaer_dir', 'cldaer_ir', 'ir_addaerosols_cams')
            if status != 0: return status

    if args.ir_addclouds or args.run_mfasis:
        if args.addsolar or args.run_mfasis:
            status = guess_dir('rttov_sccld_dir', 'cldaer_visir', 'ir_addclouds')
            if status != 0: return status
        else:
            status = guess_dir('rttov_sccld_dir', 'cldaer_ir', 'ir_addclouds')
            if status != 0: return status

    if args.run_mfasis or args.vis_scatt_model == 3:
        status = guess_dir('rttov_mfasis_cld_dir', 'mfasis_lut', 'run_mfasis or vis_scatt_model==3')
        if status != 0: return status

    if args.run_scatt:
        status = guess_dir('rttov_hydrotable_dir', 'hydrotable', 'run_scatt')
        if status != 0: return status

    if args.addpc:
        status = guess_dir('rttov_pccoeffs_dir', 'pc', 'addpc')
        if status != 0: return status

    if args.htfrtc:
        status = guess_dir('rttov_htfrtc_coeffs_dir', 'htfrtc', 'htfrtc')
        if status != 0: return status

    # Check for incompatible/missing option combinations
    if (args.ir_addclouds or args.run_mfasis) and args.run_scatt:
        print('ERROR: both VIS/IR and MW scattering options specified')
        return 1

    return 0


def write_config_file(args):
    """
    Generate a config file using supplied RadSim arguments
    """
    comment = '!'
    blank = '\n'

    def write_sep(f, n):
        f.write(comment + '-' * n + '\n')

    def write_hdr(f, s):
        write_sep(f, len(s) + 2)
        f.write(comment + ' ' + s + '\n')
        write_sep(f, len(s) + 2)
        f.write(blank)

    def write_val(f, var):
        val = getattr(args, var)
        if val is not None:
            if type(val) == str:
                f.write('  {:20s} = "{:s}"\n'.format(var, val))
            elif type(val) == float:
                f.write('  {:20s} = {:f}\n'.format(var, val))
            elif type(val) == int:
                f.write('  {:20s} = {:d}\n'.format(var, val))
            elif type(val) == bool:
                if val: f.write('  {:20s} = .true.\n'.format(var))
                else: f.write('  {:20s} = .false.\n'.format(var))
            elif type(val) == list:
                m = 20                 # m = number of values per line
                n = int(len(val) / m)  # n = number of lines to write
                if n * m != len(val): n += 1
                f.write('  {:20s} = {:s}\n'.format(var, ' '.join(map(str, val[:m]))))
                for i in range(1, n):
                    f.write(' ' * 25 + '{:s}\n'.format(' '.join(map(str, val[i*m:(i+1)*m]))))
    try:
        with open(args.config_file, 'w') as f:
            write_hdr(f, 'RadSim config file generated by ' + os.path.basename(__file__))

            f.write('&radsim_nl\n')
            f.write(blank)

            write_hdr(f, 'Input data')
            for var in ('obs_datafile', 'read_footprint_file', 'write_footprint_file',
                        'model_datafile', 'model_ancil_datafile', 'model_ancil2_datafile', 'model_filetype', 
                        'rttov_coeffs_type', 'rttov_coeffs_options',
                        'rttov_coeffs_dir', 'rttov_scaer_dir',
                        'rttov_sccld_dir', 'rttov_mfasis_cld_dir', 'rttov_hydrotable_dir',
                        'rttov_pccoeffs_dir', 'rttov_htfrtc_coeffs_dir',
                        'rttov_coeff_file', 'rttov_scaer_file',
                        'rttov_sccld_file', 'rttov_mfasis_cld_file', 'rttov_hydrotable_file',
                        'rttov_pccoeff_file', 'rttov_htfrtc_static_file', 'rttov_htfrtc_sensor_file',
                        'emiss_atlas_dir', 'brdf_atlas_dir'):
                write_val(f, var)
            f.write(blank)

            write_hdr(f, 'Satellite instrument')
            for var in ('platform', 'satid', 'inst', 'channels'):
                write_val(f, var)
            f.write(blank)

            write_hdr(f, 'General run-time options')
            for var in ('temporal_data', 'enable_footprints',
                        'run_scatt', 'run_mfasis',
                        'calc_geo_sat_angles', 'geo_sat_height',
                        'geo_sat_lat', 'geo_sat_lon',
                        'seaice_threshold', 'coast_threshold',
                        'co2_max_ppmv', 'ssu_co2_cell_pressure',
                        'cads_height_assign_threshold',
                        'default_brdf_land', 'default_brdf_seaice',
                        'output_mode', 'max_array_size', 'max_profs',
                        'nthreads', 'nprofs_per_call',
                        'use_emiss_atlas', 'use_brdf_atlas'):
                write_val(f, var)
            if args.ir_addclouds or args.run_mfasis:
                for var in ('ircloud_use_model_clw_deff', 'ircloud_use_model_ice_deff'):
                    write_val(f, var)
            if args.use_emiss_atlas: write_val(f, 'emis_atlas_id')
            if args.use_emiss_atlas or args.use_brdf_atlas: write_val(f, 'use_all_atlas_months')
            if args.obs_datafile is None: write_val(f, 'zen_angle')
            write_val(f, 'azi_angle')
            if args.addsolar:
                write_val(f, 'sun_zen_angle')
                write_val(f, 'sun_azi_angle')
            f.write(blank)

            write_hdr(f, 'RTTOV options (see RTTOV User''s Guide for more details)')
            for var in ('fastem_version', 'ir_sea_emis_model',
                        'ozone_data', 'clw_data', 'addsolar', 
                        'ir_addclouds', 'ir_addaerosols_cams',
                        'apply_reg_limits', 'addrefrac', 'plane_parallel',
                        'do_lambertian', 'lambertian_fixed_angle',
                        'do_nlte_correction', 'interp_mode',
                        'mw_clw_scheme', 'mw_clw_cloud_top',
                        'addpc', 'htfrtc', 'npcscores', 'ipcreg', 'ipcbnd'):
                write_val(f, var)
            if args.ir_addclouds or args.run_mfasis:
                for var in ('ircloud_clw_scheme', 'ircloud_ice_scheme', 'ircloud_icede_param',
                            'cldcol_threshold', 'ir_scatt_model', 'vis_scatt_model',
                            'dom_nstreams', 'dom_accuracy', 'dom_opdep_threshold'):
                    write_val(f, var)
            if args.addsolar:
                for var in ('solar_sea_brdf_model',
                            'rayleigh_single_scatt', 'rayleigh_max_wavelength', 
                            'rayleigh_min_pressure', 'dom_rayleigh'):
                    write_val(f, var)
            if args.run_scatt:
                for var in ('cc_threshold', 'ice_polarisation'):
                    write_val(f, var)
            f.write(blank)

            write_hdr(f, 'Output options')
            for var in ('output_dir', 'output_file',
                        'write_profiles', 'write_latlon',
                        'write_geom_height', 'write_trans',
                        'write_tjac', 'write_qjac', 'write_o3jac',
                        'write_tskinjac', 'write_wind10mjac', 'write_emissjac',
                        'write_radiances', 'write_emiss', 'write_brdf'):
                write_val(f, var)
            f.write(blank)

            f.write('/\n')
    except:
        print('ERROR: cannot write config file')
        return 1
    return 0


def run_radsim(args):
    """
    Run RadSim
    """
    status = subprocess.call(args.radsim_run + ' ' + args.config_file, shell=True)
    return status


if __name__ == '__main__':
    args = parse_args()

    status = check_config(args)
    if status != 0:
        print('Error in configuration options')
        sys.exit(status)

    status = write_config_file(args)
    if status != 0:
        print('Error writing RadSim config file: ' + args.config_file)
        sys.exit(status)
    print('RadSim config file written: ' + args.config_file)

    if args.write_config_only:
        sys.exit(0)

    print('Running RadSim...')
    status = run_radsim(args)
    if status != 0:
        print('Error running RadSim')
        sys.exit(status)

