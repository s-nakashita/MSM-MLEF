#!/usr/bin/env python
# -*- coding: iso-8859-1 -*-

# GUI for visualising data from RTTOV test suite output using wxPython.
# It can also be used to easily extract data to an ASCII file for processing elsewhere.
# The functions for extracting data from the test suite files are in rttov_test_plot_mod.py

# Fully compatible with RTTOV v11; mostly compatible with RTTOV v10.

# Requires:
# - Python 2.7 or Python 3.x
# - wxPython
# - matplotlib with 'wxagg' backend

# ----------------
# Guide to widgets
# ----------------

# To make plots you must first run the DIRECT code (and K code for Jacobians) for the test of interest.

# Define test inputs:
# - select tests to include in the plot using the 'Add to plot?' check buttons
# - specify RTTOV test output directories containing results to plot (e.g. tests.1.ifort/amsua/01)
# - input comma-separated profile list to plot (leave empty to plot all profiles defined in the test)
# - input comma-separated channel list to plot (leave empty to plot all available channels for each profile)
#   For bar charts ONLY the channel list from the FIRST profile in the FIRST selected test is used.
#   For PC rec. rad. Jacobians, the channel list must contain channel indices from channels_rec.txt or if blank
#   Jacobians for all reconstructed channels are plotted.
# - optionally specify comma-separated list of names for each profile which is used in the legend (unless legend text
#   is specified below)
# - optionally specify comma-separated list of names for each channel which is used in the legend (unless legend text
#   is specified below). This can be used to label channels correctly after channel extraction.

# Optionally make difference plots:
# - select "Difference plot?" to make plots of differences between test output
# - specify the test directory containing the data to subtract from all selected plots. The profile and channel
#   lists from each selected test will be used, so the difference test dir must contain output for the same
#   profiles/channels.

# Optionally calculate statistics over specified profiles for each test:
# - check the 'Stat plot' button to calculate and plot any of mean/std dev/RMS of data
#   This is particularly useful when examining differences.

# Define the plot type:
# - select plot type
# - for tau and weighting function plots: select the transmittance profile to use
# - for profile variable and Jacobian plots: select profile variable
# - for all profile plot types: select pressure/vertical coordinate for plotting
# - for all profile plot types: select whether the pressure/level is plotted on the X or Y axis
# - optionally specify the axis limits for the value being plotted: both limits must be specified to have an effect
# - optionally input labels for plot title and axes; leave blank for defaults
# - select whether a legend will be displayed or not (if no other text is supplied the defaults are based on
#   profile and channel numbers)
# - optionally provide a comma-separated list of Python colour specifiers to override the defaults.
# - set legend alpha (opacity): if slider is fully left, no legend will be displayed. Moving the slider to the
#   the right increases the legend opacity.
# - optionally enter comma-separated text for legend: this should consist of an entry for each profile or channel
#   appearing in the plot (depending on the plot type). If this is specified it overrides any other legend text.
# - optionally specify the font size for the legend text.
# - click Plot button to make the plot
# - click Write to file button to write the data to a comma-separated-value ASCII file instead of plotting it.
#   If the "Verbose output" checkbox is ticked this will insert comments in the output file to help understand
#   what has been written.

# NB If the test was run with channel extraction be aware that the channels will have been renumbered 1-n
#    and these channel numbers will be used by the GUI instead of the original channel numbers.
#    Use the "channel name" text boxes to rename channels for the legend if required.

from __future__ import print_function
import wx
import os
import copy
import string
import numpy as np
import matplotlib
matplotlib.use('wxagg')
import matplotlib.pyplot as plt
from rttov_test_plot_mod import readChannelData, readJacobians, readHydroJacobians, \
                                readPressure, readProfileVar, \
                                calcStats, calcWeightingFunction, \
                                getProfileList, getChanprof, \
                                getChanprofPCrec

# Available types of plot
PLOT_OPTS = ('Radiances',                    \
             'BTs',                          \
             'Reflectances',                 \
             'Tau total',                    \
             'Tau levels',                   \
             'Weighting function',           \
             'Jacobian',                     \
             'Jacobian - brute force',       \
             'Jacobian - PC rec.',           \
             'Surface emissivity',           \
             'Surface emissivity K',         \
             'Surface BRDF',                 \
             'Surface BRDF K',               \
             'Profile variable',             \
             'Profile variable - log scale') #, \
             #'Layer delta-pressure',         \
             #'Layer thickness')

# Radiance/BT/reflectance options
RAD_OPTS = ('Total',   \
            'Clear-sky')

# Transmittance options
TAU_OPTS = ('Tau - thermal',             \
            'Tau - solar sat-surf',      \
            'Tau - solar sun-surf-sat',  \
            'Tau - cloud')

# Options for vertical coordinate for profile plots
PAXIS_OPTS = ('Layer/level number', \
              'p (linear scale)',   \
              'p (log scale)',      \
              'Geometric height')

STAT_OPTS = ('Mean', 'StDv', 'RMS', 'Max(abs)')

# Profile variables available for plotting
PROF_VARS = ('T', 'Q', 'log(Q)', 'O3', 'CO2', 'CO', 'N2O', 'CH4', 'SO2', 'CLW', 'skin%T', 'aerosols', 'cfrac', 'cloud', 'icede', 'P', \
             'SCATT-HYDRO_FRAC', 'SCATT-RAIN', 'SCATT-SNOW', 'SCATT-GRAUPEL', 'SCATT-CLW', 'SCATT-CIW')

# Maximum number of cloud/aerosol types to display in interface
CLDAER_MAXINDEX = 13

# Colour list to use for plots (can be overridden in interface)
COLOURS = ('b', 'g', 'r', 'c', 'm', 'y', 'k', 'orange', 'olive', 'grey', 'purple', 'darkblue', 'darkgreen', 'darkred')

# Combined width of bars for each profile in bar plots (should not exceed 1.0)
BAR_WIDTH = 0.8

# --------------------------------------------------------------------


class rttov_test_plot():
    def __init__(self, ntests=3):
        self.ntests = ntests

        textwidth = 250 # Width for text controls

        # Turn on interactive plotting so showing a plot doesn't block
        plt.ion()

        # --- Main window -------------------------------------
        self.app = wx.App()
        self.mainFrame = wx.Frame(None, wx.ID_ANY, 'RTTOV test plotter', style=wx.MINIMIZE_BOX | \
                                  wx.SYSTEM_MENU | wx.CAPTION | wx.CLOSE_BOX | wx.CLIP_CHILDREN)
        # -----------------------------------------------------

        # --- Sizer for whole GUI -----------------------------
        # On the left side are the test dir and prof/channel lists for each test
        # On the right side are the plot definition controls
        # Each test and the plot definition controls are placed on their own
        # panel within a StatixBox providing a border.
        mainfgs = wx.FlexGridSizer(rows=1, cols=2, vgap=0, hgap=5)
        self.mainFrame.SetSizer(mainfgs)
        # -----------------------------------------------------

        # --- Sizer for LHS of GUI - tests --------------------
        leftfgs = wx.FlexGridSizer(rows=self.ntests+1, cols=1, vgap=5, hgap=0)
        mainfgs.Add(leftfgs, flag=wx.ALL, border=5)
        # -----------------------------------------------------

        # --- Create controls for each test -------------------
        self.btn_addtoplot = []
        self.text_testdir = []
        self.text_proflist = []
        self.text_chanlist = []
        self.text_profname = []
        self.text_channame = []

        for t in range(self.ntests):
            # --- Test panel ------------------------------------
            testpanel = wx.Panel(self.mainFrame, wx.ID_ANY)
            leftfgs.Add(testpanel)
            # ---------------------------------------------------

            # --- Sizers for the test controls ------------------
            # Box sizer for the addtoplot button
            testhbox = wx.BoxSizer(wx.HORIZONTAL)
            # Grid sizer for the remaining controls
            testfgs = wx.FlexGridSizer(rows=5, cols=2, hgap=1, vgap=1)
            # ---------------------------------------------------

            # --- Addtoplot button ------------------------------
            self.btn_addtoplot.append(wx.CheckBox(testpanel, wx.ID_ANY, 'Add to plot?'))
            testhbox.Add(self.btn_addtoplot[-1])
            if t == 0:
              self.btn_addtoplot[-1].SetValue(True)
            self.btn_addtoplot[-1].SetToolTip(wx.ToolTip( \
              'Check to include this test in the plot'))
            self.btn_addtoplot[t].Bind(wx.EVT_LEFT_DOWN, self.btn_addtoplot_clicked)
            # ---------------------------------------------------

            # --- Test directory --------------------------------
            label = wx.StaticText(testpanel, wx.ID_ANY, 'Test dir:')
            testfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
            self.text_testdir.append(wx.TextCtrl(testpanel, wx.ID_ANY, '', size=(textwidth,-1)))
            #self.text_testdir[-1].SetValue('tests.1.ifort/amsua/01') # For testing
            testfgs.Add(self.text_testdir[-1])
            self.text_testdir[-1].SetToolTip(wx.ToolTip( \
              'Test directory (e.g. tests.1.ifort/amsua/01)'))
            if not self.btn_addtoplot[-1].GetValue():
              self.text_testdir[-1].Disable()
            # ---------------------------------------------------

            # --- Profile list ----------------------------------
            label = wx.StaticText(testpanel, wx.ID_ANY, 'Profile list:')
            testfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
            self.text_proflist.append(wx.TextCtrl(testpanel, wx.ID_ANY, '1', size=(textwidth,-1)))
            testfgs.Add(self.text_proflist[-1])
            self.text_proflist[-1].SetToolTip(wx.ToolTip( \
              'Comma-separated list of profile numbers to plot; leave blank to plot all profiles defined in test'))
            if not self.btn_addtoplot[-1].GetValue():
              self.text_proflist[-1].Disable()
            # ---------------------------------------------------

            # --- Channel list ----------------------------------
            label = wx.StaticText(testpanel, wx.ID_ANY, 'Channel list:')
            testfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
            self.text_chanlist.append(wx.TextCtrl(testpanel, wx.ID_ANY, '', size=(textwidth,-1)))
            testfgs.Add(self.text_chanlist[-1])
            self.text_chanlist[-1].SetToolTip(wx.ToolTip( \
              'Comma-separated list of channel numbers to plot; leave blank to plot all channels for each profile'))
            if not self.btn_addtoplot[-1].GetValue():
              self.text_chanlist[-1].Disable()
            # ---------------------------------------------------

            # --- Profile name list -----------------------------
            label = wx.StaticText(testpanel, wx.ID_ANY, 'Prof name(s):')
            testfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
            self.text_profname.append(wx.TextCtrl(testpanel, wx.ID_ANY, '', size=(textwidth,-1)))
            testfgs.Add(self.text_profname[-1])
            self.text_profname[-1].SetToolTip(wx.ToolTip( \
              'Optional: comma-separated list of names for each specified profile; leave blank to use defaults'))
            if not self.btn_addtoplot[-1].GetValue():
              self.text_profname[-1].Disable()
            # ---------------------------------------------------

            # --- Channel name list -----------------------------
            label = wx.StaticText(testpanel, wx.ID_ANY, 'Chan name(s):')
            testfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
            self.text_channame.append(wx.TextCtrl(testpanel, wx.ID_ANY, '', size=(textwidth,-1)))
            testfgs.Add(self.text_channame[-1])
            self.text_channame[-1].SetToolTip(wx.ToolTip( \
              'Optional: comma-separated list of names for each specified channel; leave blank to use defaults; ' + \
              'integers will have "Ch" prepended (except for bar charts)'))
            if not self.btn_addtoplot[-1].GetValue():
              self.text_channame[-1].Disable()
            # ---------------------------------------------------

            # --- StaticBox to frame the test -------------------
            # Define this after the controls otherwise button tooltips don't appear...!
            testbox = wx.StaticBox(testpanel, wx.ID_ANY, 'Test ' + str(t+1))
            testboxsizer = wx.StaticBoxSizer(testbox, wx.VERTICAL)
            testboxsizer.Add(testhbox)
            testboxsizer.Add(testfgs)
            testpanel.SetSizer(testboxsizer)
            # ---------------------------------------------------


        # --- Sizer for RHS of GUI - plot defn etc ------------
        rightfgs = wx.FlexGridSizer(rows=3, cols=1, vgap=5, hgap=0)
        mainfgs.Add(rightfgs, flag=wx.TOP | wx.BOTTOM | wx.RIGHT, border=5)
        # -----------------------------------------------------


        # --- Diff test panel ---------------------------------
        diffpanel = wx.Panel(self.mainFrame, wx.ID_ANY)
        rightfgs.Add(diffpanel, flag = wx.EXPAND)
        # -----------------------------------------------------

        # --- Sizers for the diff test controls ---------------
        # Box sizer for the difference? button
        diffhbox = wx.BoxSizer(wx.HORIZONTAL)

        # Grid sizer for the text box
        difffgs = wx.FlexGridSizer(rows=2, cols=2, hgap=1, vgap=1)
        # ---------------------------------------------------

        # --- Difference? check box -------------------------
        self.btn_diffplot = wx.CheckBox(diffpanel, wx.ID_ANY, 'Difference plot?')
        diffhbox.Add(self.btn_diffplot)
        self.btn_diffplot.SetToolTip(wx.ToolTip( \
          'Check to make a difference plot'))
        self.btn_diffplot.Bind(wx.EVT_LEFT_DOWN, self.btn_diffplot_clicked)
        # ---------------------------------------------------

        # --- Diff test directory ---------------------------
        difflabel = wx.StaticText(diffpanel, wx.ID_ANY, 'Test dir:', size=label.GetSize())
        difffgs.Add(difflabel, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.text_difftestdir = wx.TextCtrl(diffpanel, wx.ID_ANY, '', size=(textwidth,-1))
        self.text_difftestdir.SetValue(self.text_testdir[0].GetValue())
        difffgs.Add(self.text_difftestdir, flag = wx.ALIGN_RIGHT)
        self.text_difftestdir.SetToolTip(wx.ToolTip( \
          'Test directory to subtract from selected tests (e.g. tests.1.ifort/amsua/01)'))
        self.text_difftestdir.Disable()
        # ---------------------------------------------------

        # --- StaticBox to frame the diff test defn ---------
        # Define this after the controls otherwise button tooltips don't appear...!
        diffbox = wx.StaticBox(diffpanel, wx.ID_ANY, 'Difference plot')
        diffboxsizer = wx.StaticBoxSizer(diffbox, wx.VERTICAL)
        diffboxsizer.Add(diffhbox)
        diffboxsizer.Add(difffgs)
        diffpanel.SetSizer(diffboxsizer)
        # -----------------------------------------------------


        # --- Stat panel --------------------------------------
        statpanel = wx.Panel(self.mainFrame, wx.ID_ANY)
        rightfgs.Add(statpanel, flag = wx.EXPAND)
        # -----------------------------------------------------

        # --- Sizers for the stat controls --------------------
        # Box sizers for the check boxes
        stathbox = wx.BoxSizer(wx.HORIZONTAL)
        statbtnhbox = wx.BoxSizer(wx.HORIZONTAL)
        # ---------------------------------------------------

        # --- Stat plot buttons -----------------------------
        self.btn_statplot = wx.CheckBox(statpanel, wx.ID_ANY, 'Plot stats?  ')
        stathbox.Add(self.btn_statplot)
        self.btn_statplot.SetToolTip(wx.ToolTip( \
          'Check to calculate and plot stats: mean/stdv/RMS calculated over all specified profiles ' + \
          'for each selected test directory'))
        self.btn_statplot.Bind(wx.EVT_LEFT_DOWN, self.btn_statplot_clicked)

        self.btn_stats = []
        self.btn_stats.append(wx.CheckBox(statpanel, wx.ID_ANY, STAT_OPTS[0]))
        stathbox.Add(self.btn_stats[-1])
        self.btn_stats[-1].SetToolTip(wx.ToolTip( \
          'Check to calculate mean'))
        self.btn_stats.append(wx.CheckBox(statpanel, wx.ID_ANY, STAT_OPTS[1]))
        stathbox.Add(self.btn_stats[-1])
        self.btn_stats[-1].SetToolTip(wx.ToolTip( \
          'Check to calculate standard deviation'))
        self.btn_stats.append(wx.CheckBox(statpanel, wx.ID_ANY, STAT_OPTS[2]))
        stathbox.Add(self.btn_stats[-1])
        self.btn_stats[-1].SetToolTip(wx.ToolTip( \
          'Check to calculate RMS'))
        self.btn_stats.append(wx.CheckBox(statpanel, wx.ID_ANY, STAT_OPTS[3]))
        stathbox.Add(self.btn_stats[-1])
        self.btn_stats[-1].SetToolTip(wx.ToolTip( \
          'Check to calculate Max(abs)'))

        for btn in self.btn_stats:
            btn.SetValue(True)
            btn.Disable()
        # ---------------------------------------------------

        # --- StaticBox to frame the stat controls ----------
        # Define this after the controls otherwise button tooltips don't appear...!
        statbox = wx.StaticBox(statpanel, wx.ID_ANY, 'Stats')
        statboxsizer = wx.StaticBoxSizer(statbox, wx.VERTICAL)
        statboxsizer.Add(stathbox)
        statpanel.SetSizer(statboxsizer)
        # -----------------------------------------------------


        # --- Plot defn panel ---------------------------------
        plotpanel = wx.Panel(self.mainFrame, wx.ID_ANY)
        rightfgs.Add(plotpanel)
        # -----------------------------------------------------

        # --- Sizer for plot defn controls --------------------
        plotfgs = wx.FlexGridSizer(rows=20, cols=2, hgap=1, vgap=1)
        # -----------------------------------------------------

        # --- Plot type ---------------------------------------
        label = wx.StaticText(plotpanel, wx.ID_ANY, 'Plot type:')
        plotfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.combo_plottype = wx.ComboBox(plotpanel, wx.ID_ANY, style=wx.CB_READONLY, size=(textwidth,-1))
        self.combo_plottype.AppendItems(PLOT_OPTS)
        self.combo_plottype.SetSelection(1)
        plotfgs.Add(self.combo_plottype)
        self.combo_plottype.SetToolTip(wx.ToolTip( \
          'Select type of plot: different options apply to each plot type'))
        # -----------------------------------------------------

        # --- Rad/BT/refl type --------------------------------
        label = wx.StaticText(plotpanel, wx.ID_ANY, 'Rad type:')
        plotfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.combo_radtype = wx.ComboBox(plotpanel, wx.ID_ANY, style=wx.CB_READONLY, size=(textwidth,-1))
        self.combo_radtype.AppendItems(RAD_OPTS)
        self.combo_radtype.SetSelection(0)
        plotfgs.Add(self.combo_radtype)
        self.combo_radtype.SetToolTip(wx.ToolTip( \
          'For radiance/BT/reflectance function plots, select type'))
        # -----------------------------------------------------

        # --- Tau type ----------------------------------------
        label = wx.StaticText(plotpanel, wx.ID_ANY, 'Tau type:')
        plotfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.combo_tautype = wx.ComboBox(plotpanel, wx.ID_ANY, style=wx.CB_READONLY, size=(textwidth,-1))
        self.combo_tautype.AppendItems(TAU_OPTS)
        self.combo_tautype.SetSelection(0)
        plotfgs.Add(self.combo_tautype)
        self.combo_tautype.SetToolTip(wx.ToolTip( \
          'For tau and weighting function plots, select transmittance type'))
        # -----------------------------------------------------

        # --- Profile variable --------------------------------
        label = wx.StaticText(plotpanel, wx.ID_ANY, 'Profile variable:')
        plotfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.combo_profvar = wx.ComboBox(plotpanel, wx.ID_ANY, style=wx.CB_READONLY, size=(textwidth,-1))
        self.combo_profvar.AppendItems(PROF_VARS)
        self.combo_profvar.SetSelection(0)
        plotfgs.Add(self.combo_profvar)
        self.combo_profvar.SetToolTip(wx.ToolTip( \
          'For profile variable and Jacobian plots, select profile variable'))
        # -----------------------------------------------------

        # --- Cld/aer type ------------------------------------
        label = wx.StaticText(plotpanel, wx.ID_ANY, 'Cld/aer type:')
        plotfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.combo_cldaertype = wx.ComboBox(plotpanel, wx.ID_ANY, style=wx.CB_READONLY, size=(textwidth,-1))
        self.combo_cldaertype.AppendItems(list(map(str, range(1, CLDAER_MAXINDEX+1))))
        self.combo_cldaertype.SetSelection(0)
        plotfgs.Add(self.combo_cldaertype)
        self.combo_cldaertype.SetToolTip(wx.ToolTip( \
          'For profile variable and Jacobian plots of cloud/aerosol, select component type index: 1-6 cloud; ' + \
          '1-13 aerosol'))
        # -----------------------------------------------------

        # --- P-axis type -------------------------------------
        label = wx.StaticText(plotpanel, wx.ID_ANY, 'P-axis:')
        plotfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.combo_paxis = wx.ComboBox(plotpanel, wx.ID_ANY, style=wx.CB_READONLY, size=(textwidth,-1))
        self.combo_paxis.AppendItems(PAXIS_OPTS)
        self.combo_paxis.SetSelection(0)
        plotfgs.Add(self.combo_paxis)
        self.combo_paxis.SetToolTip(wx.ToolTip( \
          'For profile plots, select the vertical coordinate'))
        # -----------------------------------------------------

        # --- P-axis radio buttons ----------------------------
        label = wx.StaticText(plotpanel, wx.ID_ANY, 'P/level on:')
        plotfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        paxishbox = wx.BoxSizer(wx.HORIZONTAL)
        self.btn_paxisx = wx.RadioButton(plotpanel, wx.ID_ANY, 'X-axis', style=wx.RB_GROUP)
        paxishbox.Add(self.btn_paxisx, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.btn_paxisx.SetToolTip(wx.ToolTip( \
          'For profile plots, plot pressure/levels on X axis'))
        self.btn_paxisy = wx.RadioButton(plotpanel, wx.ID_ANY, 'Y-axis')
        paxishbox.Add(self.btn_paxisy, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.btn_paxisy.SetToolTip(wx.ToolTip( \
          'For profile plots, plot pressure/levels on Y axis'))
        plotfgs.Add(paxishbox)
        self.btn_paxisy.SetValue(True)
        # ---------------------------------------------------

        # --- Value min/max ---------------------------------
        label = wx.StaticText(plotpanel, wx.ID_ANY, 'Value min/max:')
        plotfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.vlimitshbox = wx.BoxSizer(wx.HORIZONTAL)
        self.text_vmin = wx.TextCtrl(plotpanel, wx.ID_ANY, '', size=(textwidth/2,-1))
        self.vlimitshbox.Add(self.text_vmin, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.text_vmin.SetToolTip(wx.ToolTip( \
          'Set lower axis limit for value axis: upper limit must also be set'))
        self.text_vmax = wx.TextCtrl(plotpanel, wx.ID_ANY, '', size=(textwidth/2,-1))
        self.vlimitshbox.Add(self.text_vmax, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.text_vmax.SetToolTip(wx.ToolTip( \
          'Set upper axis limit for value axis: lower limit must also be set'))
        plotfgs.Add(self.vlimitshbox)
        # ---------------------------------------------------

        # --- X-axis label ------------------------------------
        label = wx.StaticText(plotpanel, wx.ID_ANY, 'X-axis label:')
        plotfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.text_xaxis_label = wx.TextCtrl(plotpanel, wx.ID_ANY, '', size=(textwidth,-1))
        plotfgs.Add(self.text_xaxis_label)
        self.text_xaxis_label.SetToolTip(wx.ToolTip( \
          'Optional: enter text for X-axis label; leave blank to use default'))
        # -----------------------------------------------------

        # --- Y-axis label ------------------------------------
        label = wx.StaticText(plotpanel, wx.ID_ANY, 'Y-axis label:')
        plotfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.text_yaxis_label = wx.TextCtrl(plotpanel, wx.ID_ANY, '', size=(textwidth,-1))
        plotfgs.Add(self.text_yaxis_label)
        self.text_yaxis_label.SetToolTip(wx.ToolTip( \
          'Optional: enter text for Y-axis label; leave blank to use default'))
        # -----------------------------------------------------

        # --- Plot title --------------------------------------
        label = wx.StaticText(plotpanel, wx.ID_ANY, 'Plot title:')
        plotfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.text_plot_title = wx.TextCtrl(plotpanel, wx.ID_ANY, '', size=(textwidth,-1))
        plotfgs.Add(self.text_plot_title)
        self.text_plot_title.SetToolTip(wx.ToolTip( \
          'Optional: enter text for plot title; leave blank to use default'))
        # -----------------------------------------------------

        # --- Colour list -------------------------------------
        label = wx.StaticText(plotpanel, wx.ID_ANY, 'Colour list:')
        plotfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.text_colours = wx.TextCtrl(plotpanel, wx.ID_ANY, '', size=(textwidth,-1))
        plotfgs.Add(self.text_colours)
        self.text_colours.SetToolTip(wx.ToolTip( \
          'Optional: comma-separated list of Python colour identifiers (e.g. r,g,b) to use in plot; ' + \
          'leave blank to use defaults'))
        # -----------------------------------------------------

        # --- Legend ------------------------------------------
        label = wx.StaticText(plotpanel, wx.ID_ANY, 'Legend alpha:')
        plotfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.slider_legend_alpha = wx.Slider(plotpanel, wx.ID_ANY, 30, size=(textwidth,-1))
        plotfgs.Add(self.slider_legend_alpha)
        self.slider_legend_alpha.SetToolTip(wx.ToolTip( \
          'Set legend opacity: 0/fully left => no legend; 1.0/fully right => fully opaque'))

        label = wx.StaticText(plotpanel, wx.ID_ANY, 'Legend text:')
        plotfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.text_legend = wx.TextCtrl(plotpanel, wx.ID_ANY, '', size=(textwidth,-1))
        plotfgs.Add(self.text_legend)
        self.text_legend.SetToolTip(wx.ToolTip( \
          'Optional: comma-separated list for legend, one entry for every dataset plotted; ' + \
          'leave blank to use defaults; takes precedence over prof/chan names above'))

        label = wx.StaticText(plotpanel, wx.ID_ANY, 'Legend fontsize:')
        plotfgs.Add(label, flag = wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.text_legend_fontsize = wx.TextCtrl(plotpanel, wx.ID_ANY, '10', size=(textwidth,-1))
        plotfgs.Add(self.text_legend_fontsize)
        self.text_legend_fontsize.SetToolTip(wx.ToolTip( \
          'Optional: font size for legend text; leave blank to use default'))
        # -----------------------------------------------------

        # --- Plot button -------------------------------------
        plotfgs.AddStretchSpacer()
        self.btn_plot = wx.Button(plotpanel, wx.ID_ANY, 'Plot')
        plotfgs.Add(self.btn_plot, flag = wx.EXPAND)
        self.btn_plot.SetToolTip(wx.ToolTip( \
          'Make plot'))
        self.btn_plot.Bind(wx.EVT_LEFT_UP, self.btn_plot_clicked)
        # -----------------------------------------------------

        # --- Write data button -------------------------------
        plotfgs.AddStretchSpacer()
        self.btn_verbose_write = wx.CheckBox(plotpanel, wx.ID_ANY, 'Verbose file output?')
        plotfgs.Add(self.btn_verbose_write)
        self.btn_verbose_write.SetToolTip(wx.ToolTip( \
          'When writing data to file add comments on profile and channel numbers etc'))

        plotfgs.AddStretchSpacer()
        self.btn_write_data = wx.Button(plotpanel, wx.ID_ANY, 'Write to file')
        plotfgs.Add(self.btn_write_data, flag = wx.EXPAND)
        self.btn_write_data.SetToolTip(wx.ToolTip( \
          'Write data to an ASCII file'))
        self.btn_write_data.Bind(wx.EVT_LEFT_UP, self.btn_write_data_clicked)
        # -----------------------------------------------------

        # --- StaticBox to frame the plot defn ----------------
        # Define this after the controls otherwise button tooltips don't appear...!
        plotbox = wx.StaticBox(plotpanel, wx.ID_ANY, 'Plot definition')
        plotboxsizer = wx.StaticBoxSizer(plotbox, wx.VERTICAL)
        plotboxsizer.Add(plotfgs)
        plotpanel.SetSizer(plotboxsizer)
        # -----------------------------------------------------

        mainfgs.Fit(self.mainFrame)
        self.mainFrame.Show()


    # --- Callbacks -----------------------------------------

    def btn_addtoplot_clicked(self, event):
        """ Enable/disable test widgets when addtoplot clicked """
        btn = event.GetEventObject()
        t = self.btn_addtoplot.index(btn)
        if btn.GetValue():
            self.btn_addtoplot[t].SetValue(False)
            self.text_testdir[t].Disable()
            self.text_proflist[t].Disable()
            self.text_chanlist[t].Disable()
            self.text_profname[t].Disable()
            self.text_channame[t].Disable()
        else:
            self.btn_addtoplot[t].SetValue(True)
            self.text_testdir[t].Enable()
            self.text_proflist[t].Enable()
            self.text_chanlist[t].Enable()
            self.text_profname[t].Enable()
            self.text_channame[t].Enable()

    def btn_diffplot_clicked(self, event):
        """ Enable/disable diff test widgets when Difference? clicked """
        btn = event.GetEventObject()
        if btn.GetValue():
            btn.SetValue(False)
            self.text_difftestdir.Disable()
        else:
            btn.SetValue(True)
            self.text_difftestdir.Enable()

    def btn_statplot_clicked(self, event):
        """ Enable/disable stat type widgets when Stat plot clicked """
        btn = event.GetEventObject()
        if btn.GetValue():
            btn.SetValue(False)
            for statbtn in self.btn_stats:
                statbtn.Disable()
        else:
            btn.SetValue(True)
            for statbtn in self.btn_stats:
                statbtn.Enable()

    def btn_plot_clicked(self, event=None):
        """ Read data and plot it """
        result = self.readData()
        if result == 0:
            self.plotData()

    def btn_write_data_clicked(self, event=None):
        """ Read data and write it to a file """
        result = self.readData()
        if result != 0:
            return

        fsel = wx.FileDialog(None, message='Choose output file', defaultDir='./',
                            style=wx.FD_SAVE | wx.FD_OVERWRITE_PROMPT)
        fsel.ShowModal()
        filename = fsel.GetPath()
        fsel.Destroy()
        if not filename:
            return

        self.writeData(filename, self.btn_verbose_write.GetValue())

    # -------------------------------------------------------

    # --- Read, plot, and write data ------------------------

    def readData(self):
        """ Read data for each test dir into self.testData[:] list. Any associated pressure
            profile data is in self.pData[:] and geometric heights are in self.geoheightData[:].
            Any necessary calculations are performed so that the contents of self.testData[:]
            are precisely the data to be plotted/written.
            This also sets a few values used specifically by the plotting function based on
            the selected data type. """

        # First process the test dir selections to obtain a list of testDirs,
        # and for each a list of profiles, channels and prof/chan labels
        self.testDirs = []
        self.profLists = []
        self.chanLists = []
        self.profNameLists = []
        self.chanNameLists = []

        for t in range(self.ntests):
            if self.btn_addtoplot[t].GetValue():
                self.testDirs.append(self.text_testdir[t].GetValue())
                if not os.path.exists(self.testDirs[-1]):
                    print('Cannot find test dir ' + self.testDirs[-1])
                    return 1

                if self.text_proflist[t].GetValue():
                    try:
                        self.profLists.append(list(map(int, self.text_proflist[t].GetValue().split(','))))
                    except:
                        print('Invalid profile list: must be comma-separated integers or empty for all profiles')
                        return 1
                else:
                    self.profLists.append(range(1, len(getProfileList(self.testDirs[-1]))+1))

                if self.text_chanlist[t].GetValue():
                    try:
                        self.chanLists.append(list(map(int, self.text_chanlist[t].GetValue().split(','))))
                    except:
                        print('Invalid channel list: must be comma-separated integers or empty for all channels')
                        return 1
                else:
                    # If no channel list is specified use chanlist from profile 1
                    if 'PC' in PLOT_OPTS[self.combo_plottype.GetSelection()]:
                        chanprof = getChanprofPCrec(self.testDirs[-1])
                    else:
                        chanprof = getChanprof(self.testDirs[-1])
                    if not chanprof:
                        return 1
                    self.chanLists.append(chanprof[0])

                # Ensure prof/chan label lists are same length as prof/chan number lists
                self.profNameLists.append(self.extendNameList(len(self.profLists[-1]), \
                                          self.text_profname[t].GetValue()))

                self.chanNameLists.append(self.extendNameList(len(self.chanLists[-1]), \
                                          self.text_channame[t].GetValue()))

        # Determine if difference test has been selected
        if self.btn_diffplot.GetValue():
            self.diffTestDir = self.text_difftestdir.GetValue()
            if not os.path.exists(self.diffTestDir):
                print('Cannot find test dir ' + self.diffTestDir)
                return 1
        else:
            self.diffTestDir = ''

        if not len(self.testDirs):
            print('No tests selected')
            return 1


        # Read the data - for each data type we need to:
        # - read data in the appropriate manner (e.g. read channel, Jacobian and/or profile data)
        # - repeat for diff test if selected
        # - check the data and diff data are OK
        # - carry out any calculations on data and diff data (e.g. weighting function, log(q) Jacobian)
        # - calculate the difference if requested
        # - calculate stats if requested
        self.iPlotData = self.combo_plottype.GetSelection()

        radOpt = self.combo_radtype.GetValue().lower()
        tauOpt = self.combo_tautype.GetValue().lower()

        self.plotType = ''
        self.pData = []
        self.geoheightData = []
        self.testData = []
        for i, t in enumerate(self.testDirs):
            if t == self.diffTestDir:
                print('Cannot diff test with itself')
                return 1
            p = np.array([])
            geoheight = np.array([])
            diffData = None
            self.vlog = False

            if PLOT_OPTS[self.iPlotData] in ['Radiances', 'BTs', 'Reflectances']:
                if PLOT_OPTS[self.iPlotData] == 'Radiances':
                    if 'total' in radOpt:
                        varName, tag = 'TOTAL', 'Radiance (mW/m$^2$/sr/cm$^{-1}$)'
                    else:
                        varName, tag = 'CLEAR', 'Clear-sky Radiance (mW/m$^2$/sr/cm$^{-1}$)'
                elif PLOT_OPTS[self.iPlotData] == 'BTs':
                    if 'total' in radOpt:
                        varName, tag = 'BT', 'BT (K)'
                    else:
                        varName, tag = 'BT_CLEAR', 'Clear-sky BT (K)'
                else:
                    if 'total' in radOpt:
                        varName, tag = 'REFL', 'Reflectance (BRF)'
                    else:
                        varName, tag = 'REFL_CLEAR', 'Clear-sky Reflectance (BRF)'
                data = readChannelData(t, 'radiance.txt', varName, self.profLists[i], self.chanLists[0])
                if self.diffTestDir:
                    diffData = readChannelData(self.diffTestDir, 'radiance.txt', varName, self.profLists[i], self.chanLists[0])
                if not self.checkData(data, diffData):
                    return 1
                self.plotType = ('barchan', tag)

            elif 'Surface' in PLOT_OPTS[self.iPlotData]:
                if PLOT_OPTS[self.iPlotData] == 'Surface emissivity':
                    varName, tag = 'EMISSIVITY_OUT', 'Surface emissivity'
                    fname = 'emissivity_out.txt'
                    model = 'direct'
                elif PLOT_OPTS[self.iPlotData] == 'Surface emissivity K':
                    varName, tag = 'EMISSIVITY_K', 'Surface emissivity Jacobian'
                    fname = 'emissivity_k.txt'
                    model = 'k'
                elif PLOT_OPTS[self.iPlotData] == 'Surface BRDF':
                    varName, tag = 'REFLECTANCE_OUT', 'Surface BRDF'
                    fname = 'reflectance_out.txt'
                    model = 'direct'
                elif PLOT_OPTS[self.iPlotData] == 'Surface BRDF K':
                    varName, tag = 'REFLECTANCE_K', 'Surface BRDF Jacobian'
                    fname = 'reflectance_k.txt'
                    model = 'k'
                data = readChannelData(t, fname, varName, self.profLists[i], self.chanLists[0], model=model)
                if self.diffTestDir:
                    diffData = readChannelData(self.diffTestDir, fname, varName, self.profLists[i], self.chanLists[0], model=model)
                if not self.checkData(data, diffData):
                    return 1
                self.plotType = ('barchan', tag)

            elif PLOT_OPTS[self.iPlotData] == 'Tau total':
                if 'thermal' in tauOpt:
                    varName = 'TAU_TOTAL'
                elif 'solar sat' in tauOpt:
                    varName = 'TAUSUN_TOTAL_PATH1'
                elif 'solar sun' in tauOpt:
                    varName = 'TAUSUN_TOTAL_PATH2'
                else:
                    varName = 'TAU_TOTAL_CLD'
                data = readChannelData(t, 'transmission.txt', varName, self.profLists[i], self.chanLists[0])
                if self.diffTestDir:
                    diffData = readChannelData(self.diffTestDir, 'transmission.txt', varName, self.profLists[i], self.chanLists[0])
                if not self.checkData(data, diffData):
                    return 1
                self.plotType = ('barchan', 'Transmittance')

            elif PLOT_OPTS[self.iPlotData] in ['Tau levels', 'Weighting function']:
                p = readPressure(t, self.profLists[i])
                nlevels = p.shape[1]
                geoheight = readChannelData(t, 'radiance.txt', 'GEOMETRIC_HEIGHT', self.profLists[i], self.chanLists[i], nlevels=nlevels)
                if 'thermal' in tauOpt:
                    varName = 'TAU_LEVELS'
                elif 'solar sat' in tauOpt:
                    varName = 'TAUSUN_LEVELS_PATH1'
                elif 'solar sun' in tauOpt:
                    varName = 'TAUSUN_LEVELS_PATH2'
                else:
                    varName = 'TAU_LEVELS_CLD'
                data = readChannelData(t, 'transmission.txt', varName, self.profLists[i], self.chanLists[i], nlevels=nlevels)
                if self.diffTestDir:
                    diffp = readPressure(self.diffTestDir, self.profLists[i])
                    diffNlevels = diffp.shape[1]
                    diffData = readChannelData(self.diffTestDir, 'transmission.txt', varName, self.profLists[i], self.chanLists[i], nlevels=diffNlevels)
                if not self.checkData(data, diffData):
                    return 1
                if PLOT_OPTS[self.iPlotData] == 'Weighting function':
                    data = calcWeightingFunction(np.log(p), data)
                    if self.diffTestDir:
                        diffData = calcWeightingFunction(np.log(diffp), diffData)
                    p = p[:,:-1]
                    if geoheight.any(): geoheight = geoheight[:,:,:-1]
                    self.plotType = ('linechan', 'd(tau) / d(log p)')
                else:
                    self.plotType = ('linechan', 'Transmittance')

            elif PLOT_OPTS[self.iPlotData] in ['Jacobian', 'Jacobian - PC rec.', 'Jacobian - brute force']:
                kdir = 'k_bf' if 'brute force' in PLOT_OPTS[self.iPlotData] else 'k'
                pcrec = 'PC' in PLOT_OPTS[self.iPlotData]
                p = readPressure(t, self.profLists[i])
                nlevels = p.shape[1]
                geoheight = readChannelData(t, 'radiance.txt', 'GEOMETRIC_HEIGHT', self.profLists[i], self.chanLists[i], nlevels=nlevels)
                varName = self.combo_profvar.GetValue()
                if varName.upper() in ['AEROSOLS', 'CLOUD']:
                    icldaer = self.combo_cldaertype.GetSelection()
                else:
                    icldaer = 0

                if varName.upper() == 'LOG(Q)':
                    # Calculate d(rad/BT) / dlog(q)
                    data = readJacobians(t, 'Q', self.profLists[i], self.chanLists[i], icldaer=icldaer, pcrec=pcrec, kdir=kdir)
                    q = readProfileVar(t, self.profLists[i], 'q.txt')
                    if self.diffTestDir:
                        diffData = readJacobians(self.diffTestDir, 'Q', self.profLists[i], self.chanLists[i], icldaer=icldaer, pcrec=pcrec, kdir=kdir)
                        diffq = readProfileVar(self.diffTestDir, self.profLists[i], 'q.txt')
                    if not self.checkData(data, diffData):
                        return 1
                    nprof, nchan, nlev = data.shape
                    for ip in range(nprof):
                        for ic in range(nchan):
                            data[ip,ic,:] = data[ip,ic,:] * q[ip]
                            if self.diffTestDir:
                                diffData[ip,ic,:] = diffData[ip,ic,:] * diffq[ip]
                else:
                    if 'SCATT-' in varName.upper():
                        kfile = 'cld_profiles_k.txt'
                        if 'HYDRO_FRAC' in varName.upper():
                            name = 'HYDRO_FRAC'
                            ihydro = 0
                        else:
                            name = 'HYDRO'
                            for ihydro, hydro in enumerate(['RAIN', 'SNOW', 'GRAUPEL', 'CLW', 'CIW']):
                                if hydro in varName.upper(): break
                        data = readHydroJacobians(t, name, self.profLists[i], self.chanLists[i], ihydro=ihydro, kdir=kdir)
                        if self.diffTestDir:
                            diffData = readHydroJacobians(self.diffTestDir, name, self.profLists[i], self.chanLists[i], ihydro=ihydro, kdir=kdir)
                    else:
                        kfile = None
                        name = varName.upper()
                        data = readJacobians(t, name, self.profLists[i], self.chanLists[i], icldaer=icldaer, pcrec=pcrec, kfile=kfile, kdir=kdir)
                        if self.diffTestDir:
                            diffData = readJacobians(self.diffTestDir, name, self.profLists[i], self.chanLists[i], icldaer=icldaer, pcrec=pcrec, kfile=kfile, kdir=kdir)
                    if not self.checkData(data, diffData):
                        return 1

                if varName.upper() in ['SKIN%T']:
                    self.plotType = ('barchan', 'Jacobian for ' + varName)
                else:
                    self.plotType = ('linechan', 'Jacobian for ' + varName)

            elif PLOT_OPTS[self.iPlotData] in ['Profile variable', 'Profile variable - log scale']:
                self.vlog = ('log scale' in PLOT_OPTS[self.iPlotData])
                p = readPressure(t, self.profLists[i])
                nlevels = p.shape[1]
                geoheight = readChannelData(t, 'radiance.txt', 'GEOMETRIC_HEIGHT', self.profLists[i], self.chanLists[i], nlevels=nlevels)
                varName = self.combo_profvar.GetValue()
                if varName.upper() in ['AEROSOLS', 'CLOUD']:
                    icldaer = self.combo_cldaertype.GetSelection()
                else:
                    icldaer = 0
                if varName.upper() in ['AEROSOLS', 'CLOUD', 'ICEDE', 'CFRAC']:
                    p = p[:,:-1]
                    if geoheight.any(): geoheight = geoheight[:,:,:-1]

                #barplot = False
                if varName.upper() == 'LOG(Q)':
                    # Plot log(q)
                    data = readProfileVar(t, self.profLists[i], 'q.txt')
                    if self.diffTestDir:
                        diffData = readProfileVar(self.diffTestDir, self.profLists[i], 'q.txt')
                    if not self.checkData(data, diffData):
                        return 1
                    data = np.log(data)
                    if self.diffTestDir:
                        diffData = np.log(diffData)
                elif varName.upper() == 'SKIN%T':
                    #data = readNameListVar(t, self.profLists[i], 'ground/skin.txt', 'k0%t ')
                    #if self.diffTestDir:
                        #diffData = readNameListVar(self.diffTestDir, self.profLists[i], 'ground/skin.txt', 'k0%t ')
                    #if not self.checkData(data, diffData):
                        #return 1
                    #barplot = True
                    print('Not yet implemented')
                    return 1
                else:
                    if varName.lower() == 'aerosols':
                        fname = 'aerosl.txt'
                    else:
                      fname = varName.lower() + '.txt'
                    column = 0
                    if varName.lower() == 'aerosols' or \
                        varName.lower() == 'cloud':
                        column = icldaer

                    data = readProfileVar(t, self.profLists[i], fname, column)
                    if self.diffTestDir:
                        diffData = readProfileVar(self.diffTestDir, self.profLists[i], fname, column)
                    if not self.checkData(data, diffData):
                        return 1

                tag = varName
                if varName.lower() == 't':
                  tag = tag + ' (K)'
                elif varName.lower() == 'p':
                  tag = tag + ' (hPa)'
                elif varName.lower() == 'cfrac':
                  tag = tag + ' (0-1)'
                elif varName.lower() == 'cloud':
                  tag = tag + ' (g/m$^3$)'
                elif varName.lower() == 'icede':
                  tag = tag + ' (micron)'
                elif varName.lower() == 'aerosols':
                  tag = tag + ' (cm$^{-3}$)'
                elif 'log' not in varName.lower():
                  tag = tag #+ ' (ppmv)'
                self.chanLists[i] = [0]
                self.plotType = ('lineprof', tag)

            else:
                print('plot type not yet implemented')
                return 1

            # Calculate diff if selected
            if self.diffTestDir:
                data -= diffData

            # Calculate stats if selected
            if self.btn_statplot.GetValue():
                stats, pList, pNameList = self.calcStatData(data)
                if stats is not None:
                    data = stats
                    self.profLists[i] = pList
                    self.profNameLists[i] = pNameList

            self.testData.append(data)
            if p.any():
                self.pData.append(p)
            if geoheight.any():
                self.geoheightData.append(geoheight / 1000.) # Convert m to km

        return 0


    def plotData(self):
        """ Generate plot from contents of self.testData[:] and (if required)
            self.pData[:] or self.geoheightData[:].
            The plot type is determined by self.plotType[:] """

        plt.figure()

        # Read widget values specific to plotting
        legAlpha = self.slider_legend_alpha.GetValue()
        if legAlpha > 0 and self.text_legend.GetValue():
            legend = self.text_legend.GetValue().split(',')
        else:
            legend = []

        try:
            legFontsize = max(float(self.text_legend_fontsize.GetValue()), 5)
        except:
            legFontsize = 14.4

        colours = COLOURS
        if self.text_colours.GetValue():
            colours = self.text_colours.GetValue().split(',')

        if self.text_vmin.GetValue() and self.text_vmax.GetValue():
            self.vlimits = (float(self.text_vmin.GetValue()), float(self.text_vmax.GetValue()))
        else:
            self.vlimits = None

        if self.plotType[0] == 'barchan':
            # These plots only use channel list from *first* selected test dir
            nchan = len(self.chanLists[0])
            xlab = 'Channel'
            ylab = self.plotType[1]
            minbt, maxbt = 999., 0.

            barplot = (nchan < 50)

            nprof = sum(list(map(len, self.profLists)))
            ind = range(nchan)
            width = BAR_WIDTH / nprof

            # Each plot statement is for a profile
            i = 0
            for it, data in enumerate(self.testData):
                for ip in range(len(self.profLists[it])):
                    if i < len(legend) and legend[i]:
                        label = legend[i]
                    else:
                        label = self.profNameLists[it][ip]
                    if not label:
                        label = 'Prof' + str(self.profLists[it][ip])
                    if barplot:
                        plt.bar(list(map(lambda x:x + i*width, ind)), data[ip][:], width, label=label, color=colours[i % len(colours)], edgecolor=['k'] * len(data[ip][:]), align='edge')
                    else:
                        plt.plot(self.chanLists[0], data[ip][:], label=label, color=colours[i % len(colours)])
                    i += 1
                minbt = min(minbt, np.min(data))
                maxbt = max(maxbt, np.max(data))

            if barplot:
                # Add any missing labels to channel name list
                for ic in range(len(self.chanNameLists[0])):
                    if not self.chanNameLists[0][ic]:
                        self.chanNameLists[0][ic] = str(self.chanLists[0][ic])

                plt.xticks(list(map(lambda x:x + 0.5 * width * nprof, ind)), self.chanNameLists[0])
                plt.xlim([0, len(self.chanLists[0]) - (1.0 - BAR_WIDTH)])
            else:
                plt.xlim((self.chanLists[0][0],self.chanLists[0][-1]))

            if self.vlimits:
                plt.ylim(self.vlimits)
            elif 'BT' in PLOT_OPTS[self.iPlotData]:
                # Fix up ylimits for BT plots
                if not self.diffTestDir:
                    margin = 10
                    plt.ylim((minbt - margin, maxbt + margin))


        elif self.plotType[0] in ['linechan', 'lineprof']:
            paxisx = self.btn_paxisx.GetValue()
            paxis_type = self.combo_paxis.GetSelection()
            if paxis_type == 1:
                plab = 'Pressure (hPa)'
                plog = False
                prev = True
            elif paxis_type == 2:
                plab = 'Pressure (hPa)'
                plog = True
                prev = True
            elif paxis_type == 3:
                plab = 'Geometric height (km)'
                plog = False
                prev = False
            else:
                plab = 'Levels' # or layers in some cases
                plog = False
                prev = True
            vlab = self.plotType[1]

            minp, maxp = 9999., 0.
            i = 0
            for it, data in enumerate(self.testData):
                for ip in range(len(self.profLists[it])):
                    for ic in range(len(self.chanLists[it])):

                        # Sort out label: complicated, but tries to do something useful
                        if i < len(legend) and legend[i]:
                            # Any legend entry takes precedence
                            label = legend[i]
                        else:
                            plabel = ''
                            if len(self.profLists[it]) > 1 or self.plotType[0] == 'lineprof':
                                # Prof label omitted if there's only one profile (except for profile plots)
                                plabel = self.profNameLists[it][ip]
                                if not plabel:
                                    plabel = 'Prof' + str(self.profLists[it][ip])
                            clabel = ''
                            if self.plotType[0] == 'linechan':
                                # Don't include chan label for profile plots
                                clabel = self.chanNameLists[it][ic]
                                if not clabel:
                                    clabel = 'Ch' + str(self.chanLists[it][ic])
                            if plabel and clabel:
                                label = plabel + ' - ' + clabel
                            else:
                                label = plabel + clabel

                        v = data[ip][ic][:] if self.plotType[0] == 'linechan' else data[ip][:]
                        if paxis_type == 1:
                            p = self.pData[it][ip]
                        elif paxis_type == 2:
                            p = self.pData[it][ip]
                        elif paxis_type == 3:
                            p = self.geoheightData[it][ip][ic]
                        else:
                            p = range(len(data[ip][ic][:])) if self.plotType[0] == 'linechan' else range(len(data[ip][:]))
                        minp = min(minp, min(p))
                        maxp = max(maxp, max(p))

                        (a, b) = (p, v) if paxisx else (v, p)
                        if plog and self.vlog:
                            plt.loglog(a, b, label=label, color=colours[i % len(colours)])
                        elif (paxisx and plog and not self.vlog) or (not paxisx and not plog and self.vlog):
                            plt.semilogx(a, b, label=label, color=colours[i % len(colours)])
                        elif (paxisx and not plog and self.vlog) or (not paxisx and plog and not self.vlog):
                            plt.semilogy(a, b, label=label, color=colours[i % len(colours)])
                        else:
                            plt.plot(a, b, label=label, color=colours[i % len(colours)])
                        i += 1

            if paxisx:
                if prev:
                    plt.xlim((minp, maxp))
                else:
                    plt.xlim((maxp, minp))
                xlab, ylab = plab, vlab
                if self.vlimits: plt.ylim(self.vlimits)
            else:
                if prev:
                    plt.ylim((maxp, minp))
                else:
                    plt.ylim((minp, maxp))
                xlab, ylab = vlab, plab
                if self.vlimits: plt.xlim(self.vlimits)

        # Add axis labels and legend - same for all plot types
        if self.text_xaxis_label.GetValue():
            plt.xlabel(self.text_xaxis_label.GetValue())
        else:
            plt.xlabel(xlab)

        if self.text_yaxis_label.GetValue():
            plt.ylabel(self.text_yaxis_label.GetValue())
        else:
            plt.ylabel(ylab)

        if self.text_plot_title.GetValue():
            plt.title(self.text_plot_title.GetValue().replace('\\n', '\n'))
        #else:
            #plt.title(title)

        if legAlpha > 0:
            plt.legend(loc='best', prop={'size':legFontsize}, framealpha=legAlpha/100.0)
        plt.show()

        return 0


    def writeData(self, filename, verbose=False):
        """ Write contents of self.testData to file, optionally with comments """

        with open(filename, 'w') as f:
            for it, data in enumerate(self.testData):
                if verbose:
                    f.write('# Test dir: ' + self.testDirs[it] + '\n')
                if self.plotType[0] == 'barchan':
                    if verbose:
                        f.write('# Chan list: ' + ','.join(list(map(str, self.chanLists[0]))) + '\n')
                    for ip in range(len(self.profLists[it])):
                        if verbose:
                            f.write('# Prof: ' + str(self.profLists[it][ip]) + '\n')
                        f.write(','.join(list(map(str, data[ip]))) + '\n')
                elif self.plotType[0] == 'lineprof':
                    for ip in range(len(self.profLists[it])):
                        if verbose:
                            f.write('# Prof: ' + str(self.profLists[it][ip]) + '\n')
                        f.write(','.join(list(map(str, data[ip]))) + '\n')
                elif self.plotType[0] == 'linechan':
                    for ip in range(len(self.profLists[it])):
                        if verbose:
                            f.write('# Prof: ' + str(self.profLists[it][ip]) + '\n')
                        for ic in range(len(self.chanLists[it])):
                            if verbose:
                                f.write('# Chan: ' + str(self.chanLists[it][ic]) + '\n')
                            f.write(','.join(list(map(str, data[ip][ic]))) + '\n')
        return 0
    # -------------------------------------------------------

    # --- Helper functions ----------------------------------

    def extendNameList(self, nref, inputString):
        """ Extend a list of names (CSV in a string) to length nref """
        nameList = inputString.split(',')
        if len(nameList) >= nref:
            return nameList[:nref]
        else:
            if inputString == '':
                nameList = []
            nameList.extend([''] * (nref - len(nameList)))
            return nameList

    def checkData(self, data, diffData=None):
        """ Carry out common checks on data """
        if len(data) == 0:
            print('Error reading data')
            return False
        if diffData is not None:
            if len(diffData) == 0:
                print('Error reading difference data')
                return False
            if data.shape != diffData.shape:
                print('Data/diff data size mismatch: different number of levels?')
                return False
        return True

    def calcStatData(self, data):
        """ Return selected stats and appropriate profile and label lists """
        selStats = list(map(lambda x: int(x.GetValue()), self.btn_stats))
        if sum(selStats) == 0:
            # No stats selected
            return (None, ) * 3

        dims = data.shape
        if dims[0] == 1:
            # Need more than 1 profile to calculate stats
            return (None, ) * 3

        stats = calcStats(data)

        profNameList = []
        result = np.zeros((sum(selStats),) + dims[1:])
        j = 0
        for i in range(len(STAT_OPTS)):
            if selStats[i]:
                profNameList.append(STAT_OPTS[i])
                result[j] = stats[i]
                j += 1
        return result, range(j), profNameList

    # -------------------------------------------------------


if __name__ == '__main__':
    gui = rttov_test_plot(ntests=3)
    gui.app.MainLoop()

