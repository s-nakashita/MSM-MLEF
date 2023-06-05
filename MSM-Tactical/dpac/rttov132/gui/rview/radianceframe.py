# -*- coding: utf-8 -*-
import datetime
import h5py
import os
import sys
import getopt
import time
import rttov
from rview import util
import wx
import numpy
import time
import re
import random

import matplotlib
import matplotlib.pyplot as plt
from matplotlib.backends.backend_wxagg import FigureCanvasWxAgg as FigureCanvas
from matplotlib.backends.backend_wxagg import \
    NavigationToolbar2WxAgg as ToolBar
from matplotlib.ticker import MaxNLocator
from rview import colors
from rview.myunits import *
import warnings
warnings.filterwarnings("ignore")
fh5 = []
spl = 29979245800.0  # en cm


class figPlot(plt.Figure):

    def __init__(self):
        plt.Figure.__init__(self)
        a1 = self.add_subplot(321, title='')
        a2 = self.add_subplot(322, title='', sharex=a1)
        a3 = self.add_subplot(323, title='', sharex=a1)
        a4 = self.add_subplot(324, title='', sharex=a1)
        a5 = self.add_subplot(325, title='', sharex=a1)
        a6 = self.add_subplot(326, title='', sharex=a1)


class figPlot0(plt.Figure):

    def __init__(self):
        plt.Figure.__init__(self)
        a1 = self.add_subplot(221, title='')
        a2 = self.add_subplot(222, title='', sharex=a1)
        a3 = self.add_subplot(223, title='', sharex=a1)
        a4 = self.add_subplot(224, title='', sharex=a1)


class figPlot111(plt.Figure):

    def __init__(self):
        plt.Figure.__init__(self)
        a1 = self.add_subplot(111, title='')
        a1.twinx()


class RadianceFrame(util.GenericView):

    helpTitle = "Help"
    helpMessage = """Run direct"""
    helpPage = os.environ["RTTOV_GUI_PREFIX"] + "/doc/helpRadianceFrame.html"

    def __init__(self, parent, title, fname, addsolar):
        util.GenericView.__init__(self, parent,  title)


# default menu
        self.deb = time.time()
        self.CreateMenuBar()
        self.SetSize((900, 780))
        self.SetPosition((10, 10))

# timer StatusBar
        self.txtsb = ''
        self.timer = wx.Timer(self)
        self.Bind(wx.EVT_TIMER, self.refreshSB, self.timer)

# display reflectance flags
        self.solar = 0
        self.disRefl = 1
        self.drwRefl = 1

# flag for handling UV case (ex GOME)
        self.isUVVIS = False

# run ref for "pseudo run"
        self.refRun = 1

        self.nbChn = 0
        self.nbPChn = 0
        self.nbRun = 0
        self.nbPRun = 0
        self.lstChn = []
        self.lstWv = []

        self.taby = {}

        self.tbtr = {}
        self.tbtc = {}

        self.prtotal = []
        self.prclear = []
        self.prbt = []
        self.prbtclear = []
        self.prrefl = []
        self.prreflclear = []

        self.runlist = ['-- Run --']
        self.prunlist = ['-- Pseudo run --']
        self.prunlist111 = ['-- Pseudo run --']

        self.chnlist = ['-- Channel --']
        self.pchnlist = ['-- Pseudo channel --']

# choice for reference run for the pseudo runs
        self.refrunlist = ['-- Ref run --']

# plot list
        self.pltlistall = ['BT', 'BT_CLEAR',
                           'TOTAL', 'CLEAR', 'REFL', 'REFL_CLEAR']
        self.pltlistir = ['BT', 'BT_CLEAR', 'TOTAL', 'CLEAR']
        self.pltlistuv = ['TOTAL', 'CLEAR', 'REFL', 'REFL_CLEAR']
        self.pltlist = []

        self.units_d = {}

        self.units_d['TOTAL'] = radianceUnit
        self.units_d['CLEAR'] = radianceUnit
        self.units_d['BT'] = 'K'
        self.units_d['BT_CLEAR'] = 'K'
        self.units_d['REFL'] = ''
        self.units_d['REFL_CLEAR'] = ''
        self.units_label = {'TOTAL': radianceLabel,
                            'CLEAR': radianceLabel,
                            'BT': 'K',
                            'BT_CLEAR': 'K',
                            'REFL': '',
                            'REFL_CLEAR': ''}
        self.mz = 0
        self.cols_d = {}
        self.cols_d['TOTAL'] = 'bs-'
        self.cols_d['CLEAR'] = 'bo-'
        self.cols_d['BT'] = 'gs-'
        self.cols_d['BT_CLEAR'] = 'go-'
        self.cols_d['REFL'] = 'cs-'
        self.cols_d['REFL_CLEAR'] = 'co-'
        self.txtcol = {}
        self.txtcol['TOTAL'] = colors.radFrameRadTotal
        self.txtcol['CLEAR'] = colors.radFrameRadClear
        self.txtcol['BT'] = colors.radFrameBT
        self.txtcol['BT_CLEAR'] = colors.radFrameBTClear
        self.txtcol['REFL'] = colors.radFrameRefl
        self.txtcol['REFL_CLEAR'] = colors.radFrameReflClear


# window creation before read file: minimum window with active status bar
# 2  tabs : Runs et Channels
        self.nbk = wx.Notebook(
            self, -1, wx.DefaultPosition, wx.DefaultSize, wx.NB_TOP)

        bxsgen = wx.BoxSizer()
        bxsgen.Add(self.nbk, 1, wx.EXPAND | wx.ALL, 5)
        self.SetSizer(bxsgen)
#
#  statusbar
        self.Centre(wx.BOTH)
        self.sbgen = self.CreateStatusBar()
        self.sbgen.SetBackgroundColour('WHITE')
        txt = 'Satellite=MySat Instrument=MyIns'
        self.sbgen.SetStatusText(txt)

        self.Layout()

# read radiances H5
        self.pccomp = numpy.zeros(100)
        self.read_rad_h5(fname, addsolar)

# mook-up for serveral runs (read next files) for "standalone" test
        for f in fh5[2:]:
            print("----f= ", f)
            print("simul display Refl=True")
            self.disRefl = 0
            self.read_rad_h5(f, 0)
        self.Show()

    def initPlot111(self):

        pnl = wx.Panel(self.nbk, wx.ID_ANY, wx.DefaultPosition,
                       wx.DefaultSize, wx.TAB_TRAVERSAL)
        self.nbk.AddPage(pnl, "OnePlot", True)
        bxsv = wx.BoxSizer(wx.VERTICAL)
        self.spb111 = wx.Panel(pnl)
        self.bxsh111 = wx.BoxSizer(wx.HORIZONTAL)

# choice : plots
        self.chxplt111 = wx.ComboBox(
            self.spb111, choices=self.pltlist, style=wx.CB_READONLY)
        self.chxplt111.SetSelection(0)
        self.chxplt111.Bind(
            wx.EVT_COMBOBOX, lambda event: self.pltChoix111(event, 'type'))
        self.bxsh111.Add(self.chxplt111)

# choice : runlist init in initPlotRun and update in addRun
        self.chxrun111 = wx.ComboBox(
            self.spb111, choices=self.runlist, style=wx.CB_READONLY)
        self.chxrun111.SetSelection(1)
        self.chxrun111.Bind(
            wx.EVT_COMBOBOX, lambda event: self.pltChoix111(event, 'run'))
        self.bxsh111.Add(self.chxrun111)

# choice : reference run
        self.chxrefrun111 = wx.ComboBox(
            self.spb111, choices=self.refrunlist, style=wx.CB_READONLY)
        self.chxrefrun111.SetSelection(0)
        self.chxrefrun111.Bind(
            wx.EVT_COMBOBOX, lambda event: self.refrunChoix(event, '111'))
        self.bxsh111.Add(self.chxrefrun111)


# choice : prunlist update in addRun
        self.chxprun111 = wx.ComboBox(
            self.spb111, choices=self.prunlist111, style=wx.CB_READONLY)
        self.chxprun111.SetSelection(0)
        self.chxprun111.Bind(
            wx.EVT_COMBOBOX, lambda event: self.pltChoix111(event, 'prun'))
        self.bxsh111.Add(self.chxprun111)

        self.spb111.SetSizer(self.bxsh111)
        self.spb111.Layout()


# choice : chnlist init in initPlotChn
        if self.nbChn < 50:
            self.chxchn111 = wx.ComboBox(
                self.spb111, choices=self.chnlist, style=wx.CB_READONLY)
            self.chxchn111.SetSelection(0)
            self.chxchn111.Bind(
                wx.EVT_COMBOBOX, lambda event: self.pltChoix111(event, 'chn'))
            self.bxsh111.Add(self.chxchn111)

# choixce : pchnlist update in addPChn
        if self.nbChn < 50:
            self.chxpchn111 = wx.ComboBox(
                self.spb111, choices=self.pchnlist, style=wx.CB_READONLY)
            self.chxpchn111.SetSelection(0)
            self.chxpchn111.Bind(
                wx.EVT_COMBOBOX, lambda event: self.pltChoix111(event, 'pchn'))
            self.bxsh111.Add(self.chxpchn111)

# end combobox 111

        bxsv.Add(self.spb111)
        self.fig111 = figPlot111()
        cnv = FigureCanvas(pnl, -1, self.fig111)
        cnv.mpl_connect('motion_notify_event', self.onMouseMotion)
#
#  matplotlib toolbar (zoom..)
        self.tlb111 = ToolBar(cnv)
        self.tlb111.Realize()

        bxsv.Add(cnv,  -1, wx.EXPAND | wx.ALL, 5)
        bxsv.Add(self.tlb111)

        pnl.SetSizer(bxsv)
        pnl.Layout()

    def pltChoix111(self, evt, chx):
        # reset toolbar history
        self.tlb111.Refresh()

        selplt = self.chxplt111.GetCurrentSelection()
        selrun = self.chxrun111.GetCurrentSelection() - 1
        selprun = self.chxprun111.GetCurrentSelection() - 1

        if self.nbChn < 50:
            selchn = self.chxchn111.GetCurrentSelection() - 1
            selpchn = self.chxpchn111.GetCurrentSelection() - 1
        else:
            selchn = -1
            selpchn = -1

        if chx != 'type':
            if chx != 'run':
                self.chxrun111.SetSelection(0)
                selrun = -1
            if chx != 'prun':
                self.chxprun111.SetSelection(0)
                selprun = -1
            if chx != 'chn' and self.nbChn < 50:
                self.chxchn111.SetSelection(0)
                selchn = -1
            if chx != 'pchn' and self.nbChn < 50:
                self.chxpchn111.SetSelection(0)
                selpchn = -1

        self.redraw111(selplt, selrun, selprun, selchn, selpchn)

    def reinitPRun(self, iref):
        if iref == 0:
            return

        txt = "reinit pseudo runs with ref=run_%02d" % iref
        self.writeSB(txt, 'YELLOW', 0, 1)

        self.refRun = iref
        self.prunlist = ['-- Pseudo run --']
        self.prunlist111 = ['-- Pseudo run --']
        self.chxprun.Clear()
        self.chxprun.Append('-- Pseudo run --')
        self.chxprun.SetSelection(0)
        self.chxprun111.Clear()
        self.chxprun111.Append('-- Pseudo run --')
        self.chxprun111.SetSelection(0)

        for i in range(1, self.nbRun + 1):
            if i != iref:
                nprun = 'run_%02d minus run_%02d' % (i, iref)
# init taby : diff ref-i
                self.taby['TOTAL_( %s )' % nprun] = self.ttotal[
                    i - 1][:] - self.ttotal[iref - 1][:]
                self.taby['CLEAR_( %s )' % nprun] = self.tclear[
                    i - 1][:] - self.tclear[iref - 1][:]
                self.taby['BT_( %s )' % nprun] = self.tbt[
                    i - 1][:] - self.tbt[iref - 1][:]
                self.taby['BT_CLEAR_( %s )' % nprun] = self.tbtclear[
                    i - 1][:] - self.tbtclear[iref - 1][:]
                self.taby['REFL_( %s )' % nprun] = self.trefl[
                    i - 1][:] - self.trefl[iref - 1][:]
                self.taby['REFL_CLEAR_( %s )' % nprun] = self.treflclear[
                    i - 1][:] - self.treflclear[iref - 1][:]
                self.prunlist.append(nprun)
                self.prunlist111.append(nprun)
                self.chxprun111.Append(nprun)
                self.chxprun.Append(nprun)

                nprun = 'run_%02d versus run_%02d' % (i, iref)
# init taby : i
                self.taby['TOTAL_( %s )' % nprun] = self.ttotal[i - 1][:]
                self.taby['CLEAR_( %s )' % nprun] = self.tclear[i - 1][:]
                self.taby['BT_( %s )' % nprun] = self.tbt[i - 1][:]
                self.taby['BT_CLEAR_( %s )' % nprun] = self.tbtclear[i - 1][:]
                self.taby['REFL_( %s )' % nprun] = self.trefl[i - 1][:]
                self.taby['REFL_CLEAR_( %s )' % nprun] = self.treflclear[
                    i - 1][:]

                self.prunlist111.append(nprun)
                self.chxprun111.Append(nprun)

    def majSubplot(self, oplot, typAxe, typPlot, ii, ix):
        label = '%s_%s_%02d' % (typPlot, typAxe, ii)
        if typAxe == 'prun':
            label = '%s_( %s )' % (typPlot, self.prunlist[ii])
        elif typAxe == 'pchn':
            label = '%s_( formula_%02d )' % (typPlot, ii)

        tx = numpy.arange(1, ix)
        tx1 = tx
# X axe in WN
        if self.nbChn >= 50 and not self.isUVVIS:
            tx1 = self.lstWv
# count number of NaN
        nbnan = sum(
            bool(numpy.isnan(self.taby[label][n]))
            for n in range(len(self.taby[label]))
        )

        nplt = oplot
        nplt.cla()
        nplt.set_title(label)

        if typAxe in ['run', 'prun']:
            if self.nbChn >= 50 and not self.isUVVIS:
                nplt.set_xlabel(wavenumberLabel)
            else:
                nplt.set_xlabel('channels')
        elif typAxe in ['chn', 'pchn']:
            nplt.set_xlabel('runs')
# no plot if all result is NaN
        if nbnan < len(tx1):
            nplt.plot(tx1, self.taby[label], self.cols_d[
                      typPlot], markersize=self.mz, label='')
            nplt.set_xlim(numpy.nanmin(tx1), numpy.nanmax(tx1))
        else:
            txt = "%s : Nan" % label
            self.writeSB(txt, 'YELLOW', 5, 1)

        nplt.grid(True, axis='both')
        nplt.set_ylabel(self.units_label[typPlot])
        nplt.xaxis.set_major_locator(MaxNLocator(integer=True))
        return nplt

    def majSubplot111(self, oplot, otwinx, typAxe, typPlot, ii, ix):
        """ update the One plot graph
            oplot : first axe
            otwinx : second axe
            typeAxe: type of graph (run, pcrun, chn)
            ii:
            ix: """
        label111 = '%s_%s_%02d' % (typPlot, typAxe, ii)
        oklabel = 0
        legend1 = ''
        legend2 = ''

        if typAxe == 'run':
            nr = int('%s' % label111.split('_')[-1])
            if self.pccomp[nr]:
                oklabel = 1
                legend1 += ('Run_%02d PC' % nr)
            else:
                legend1 += ('Run_%02d' % nr)

        elif typAxe == 'prun':
            label111 = '%s_( %s )' % (typPlot, self.prunlist111[ii])

            lab1 = '%s' % label111.split(' ')[1]
            nr1 = int('%s' % lab1.split('_')[-1])
            lab2 = '%s' % label111.split(' ')[3]
            nr2 = int('%s' % lab2.split('_')[-1])
            legend1 += 'Run_%02d' % nr1
            legend2 += 'Diff Run_%02d - Run_%02d' % (nr1, nr2)

            if self.pccomp[nr1] and self.pccomp[nr2]:
                oklabel = 1
                legend2 += '\n              PC - PC'
            elif self.pccomp[nr1]:
                oklabel = 1
                legend2 += '\n              PC - Reg'
            elif self.pccomp[nr2]:
                oklabel = 1
                legend2 += '\n            Reg - PC'

        elif typAxe == 'pchn':
            label111 = '%s_( formula_%02d )' % (typPlot, ii)

        tx = numpy.arange(1, ix)
        tx1 = tx
# X axe in WN
        if self.nbChn >= 50 and not self.isUVVIS:
            tx1 = self.lstWv

        nplt = oplot
        nplt.cla()
        ntwinx = otwinx
        ntwinx.cla()

        nplt.set_title(label111)
        ntwinx.set_title(label111)

        if typAxe in ['run', 'prun']:
            if self.nbChn >= 50 and not self.isUVVIS:
                nplt.set_xlabel(wavenumberLabel)
            else:
                nplt.set_xlabel('channels')
        elif typAxe in ['chn', 'pchn']:
            nplt.set_xlabel('runs')

        if typAxe == 'prun':
            ref = '%s_%s' % (typPlot, label111.split(' ')[3])
            run = '%s_%s' % (typPlot, label111.split(' ')[1])

            labref = '%s (%s)' % (ref, self.units_label[typPlot])
            nplt.plot(tx1, self.taby[ref], self.cols_d[
                      typPlot], markersize=self.mz)
            nplt.set_xlim(numpy.nanmin(tx1), numpy.nanmax(tx1))
            nplt.set_ylabel(labref)

            if re.search('minus', label111):
                labrun = '%s minus %s (%s)' % (run, ref,
                                               self.units_label[typPlot])
                ntwinx.plot(tx1, self.taby[label111], 'r-')
                ntwinx.set_ylabel(self.units_d[typPlot], color='r')
                ntwinx.set_ylabel(labrun)
                ntwinx.tick_params(
                    right=True, left=False,
                    labelleft=False, labelright=True, colors="red")
            else:
                labrun = '%s (%s)' % (run, self.units_label[typPlot])
                nplt.plot(tx1, self.taby[run], '-r')
                ntwinx.set_ylabel(self.units_d[typPlot], color='r')
                ntwinx.set_ylabel(labrun)
                for tl in ntwinx.get_yticklabels():
                    tl.set_visible(False)

        else:
            nplt.plot(tx1, self.taby[label111], self.cols_d[
                      typPlot], markersize=self.mz)
            nplt.set_xlim(numpy.nanmin(tx1), numpy.nanmax(tx1))
            ylabel = '%s (%s)' % (label111, self.units_label[typPlot])
            nplt.set_ylabel(ylabel)
            for tl in ntwinx.get_yticklabels():
                tl.set_visible(False)

        nbticks = len(nplt.get_xticks())

        if oklabel:
            txtbox = dict(boxstyle='round', facecolor='white', alpha=1)
            nplt.text(0.015, 0.99, legend1, color=self.txtcol[typPlot],
                      transform=nplt.transAxes,
                      fontsize=14,
                      verticalalignment='bottom',
                      bbox=txtbox)
            nplt.text(0.80, 0.98, legend2, color='red',
                      transform=nplt.transAxes,
                      fontsize=12,
                      verticalalignment='bottom',
                      bbox=txtbox)

        nplt.grid(True, axis='both')
        nplt.xaxis.set_major_locator(MaxNLocator(integer=True))
        return nplt

    def redraw111(self, i_plt, i_run, i_prun, i_chn, i_pchn):
        taxes = self.fig111.get_axes()
        if i_run >= 0:
            taxes[0] = self.majSubplot111(taxes[0], taxes[1], 'run',
                                          self.pltlist[i_plt],
                                          i_run + 1, self.nbChn + 1)
        elif i_prun >= 0:
            taxes[0] = self.majSubplot111(taxes[0], taxes[1], 'prun',
                                          self.pltlist[i_plt],
                                          i_prun + 1, self.nbChn + 1)
        elif i_chn >= 0:
            taxes[0] = self.majSubplot111(taxes[0], taxes[1], 'chn',
                                          self.pltlist[i_plt],
                                          i_chn + 1, self.nbRun + 1)
        elif i_pchn >= 0:
            taxes[0] = self.majSubplot111(taxes[0], taxes[1],
                                          'pchn', self.pltlist[
                                          i_plt], i_pchn + 1,
                                          self.nbRun + 1)

        self.fig111.tight_layout()
        self.fig111.canvas.draw()

    def initPlotRuns(self, irun):
        pnl = wx.Panel(self.nbk, wx.ID_ANY, wx.DefaultPosition,
                       wx.DefaultSize, wx.TAB_TRAVERSAL)

        if irun >= 0:
            self.nbk.InsertPage(irun, pnl, "byRun", False)
        else:
            self.nbk.AddPage(pnl, "byRun", False)

        bxsv = wx.BoxSizer(wx.VERTICAL)

        self.spbr = wx.Panel(pnl)
        self.bxshr = wx.BoxSizer(wx.HORIZONTAL)
# 1 x button by run

        if self.nbRun == 1:
            self.runlist.append('run_01')
# init taby
            self.taby['TOTAL_run_01'] = self.ttotal[0][:]
            self.taby['CLEAR_run_01'] = self.tclear[0][:]
            self.taby['BT_run_01'] = self.tbt[0][:]
            self.taby['BT_CLEAR_run_01'] = self.tbtclear[0][:]
            self.taby['REFL_run_01'] = self.trefl[0][:]
            self.taby['REFL_CLEAR_run_01'] = self.treflclear[0][:]

        self.chxrun = wx.ComboBox(
            self.spbr, choices=self.runlist, style=wx.CB_READONLY)
        self.chxrun.SetSelection(1)
        self.chxrun.Bind(wx.EVT_COMBOBOX, self.runChoix)
        self.bxshr.Add(self.chxrun)

# reference run choice
        self.chxrefrun = wx.ComboBox(
            self.spbr, choices=self.refrunlist, style=wx.CB_READONLY)
        if len(self.refrunlist) >= 2:
            self.chxrefrun.SetSelection(1)
        self.chxrefrun.Bind(
            wx.EVT_COMBOBOX, lambda event: self.refrunChoix(event, 'run'))
        self.bxshr.Add(self.chxrefrun)

# 1 x button by Run(i) - Run(i-1)
        self.chxprun = wx.ComboBox(
            self.spbr, choices=self.prunlist, style=wx.CB_READONLY)
        self.chxprun.SetSelection(0)
        self.chxprun.Bind(wx.EVT_COMBOBOX, self.runPChoix)
        self.bxshr.Add(self.chxprun)

        self.spbr.SetSizer(self.bxshr)
        self.spbr.Layout()

        bxsv.Add(self.spbr)
        if self.isUVVIS or self.drwRefl == 0:
            self.rfig = figPlot0()  # 4 graphs
        elif self.disRefl == 0:
            self.rfig = figPlot0()
        else:
            self.rfig = figPlot()  # 6 graphs

        cnv = FigureCanvas(pnl, -1, self.rfig)
        cnv.mpl_connect('motion_notify_event', self.onMouseMotion)
#
# toolbar :  matplotlib  utilities (zoom...)
        tlb = ToolBar(cnv)
        tlb.Realize()

        bxsv.Add(cnv,  -1, wx.EXPAND | wx.ALL, 5)
        bxsv.Add(tlb)

        pnl.SetSizer(bxsv)
        pnl.Layout()

    def refrunChoix(self, evt, page):
        if page == '111':
            selrefrun = self.chxrefrun111.GetCurrentSelection()
        elif page == 'run':
            selrefrun = self.chxrefrun.GetCurrentSelection()
        else:
            return

        self.reinitPRun(selrefrun)
        self.chxrefrun.SetSelection(selrefrun)
        self.chxrefrun111.SetSelection(selrefrun)

    def runChoix(self, evt):
        self.chxprun.SetSelection(0)
        sel = self.chxrun.GetCurrentSelection()
        self.redrawRun(sel)

    def runPChoix(self, evt):
        self.chxrun.SetSelection(0)
        sel = self.chxprun.GetCurrentSelection()
        self.redrawPRun(sel)

    def addRuns(self):

        irun = self.nbRun
        if (irun == 2):
            self.refrunlist.append('ref run = 01')
            self.chxrefrun.Append('ref run = 01')
            self.chxrefrun111.Append('ref run = 01')

        nrun = 'run_%02d' % self.nbRun
        self.runlist.append(nrun)
        self.chxrun.Append(nrun)
        self.chxrun111.Append(nrun)
        self.chxrun.SetSelection(irun)
        self.chxrun111.SetSelection(irun)

# update taby
        self.taby['TOTAL_run_%02d' % irun] = self.ttotal[irun - 1][:]
        self.taby['CLEAR_run_%02d' % irun] = self.tclear[irun - 1][:]
        self.taby['BT_run_%02d' % irun] = self.tbt[irun - 1][:]
        self.taby['BT_CLEAR_run_%02d' % irun] = self.tbtclear[irun - 1][:]
        self.taby['REFL_run_%02d' % irun] = self.trefl[irun - 1][:]
        self.taby['REFL_CLEAR_run_%02d' % irun] = self.treflclear[irun - 1][:]

# update pruns
        self.reinitPRun(irun - 1)

        self.refrunlist.append('ref run = %02d' % (self.nbRun))
        self.chxrefrun.Append('ref run = %02d' % (self.nbRun))
        self.chxrefrun111.Append('ref run = %02d' % (self.nbRun))

    def redrawRun(self, i_run):
        if i_run == 0:
            return

        taxes = self.rfig.get_axes()
        for iplt in range(len(self.pltlist)):
            taxes[iplt] = self.majSubplot(taxes[iplt], 'run', self.pltlist[
                                          iplt], i_run, self.nbChn + 1)

        self.rfig.tight_layout()
        self.rfig.canvas.draw()

    def redrawPRun(self, i_prun):
        if i_prun == 0:
            return

        taxes = self.rfig.get_axes()
        for iplt in range(len(self.pltlist)):
            taxes[iplt] = self.majSubplot(taxes[iplt], 'prun',  self.pltlist[
                                          iplt], i_prun, self.nbChn + 1)

        self.rfig.tight_layout()
        self.rfig.canvas.draw()

    def initPlotChns(self, ichn):
        if not self.byChn:
            return
#
        pnl = wx.Panel(self.nbk, wx.ID_ANY, wx.DefaultPosition,
                       wx.DefaultSize, wx.TAB_TRAVERSAL)

        if ichn >= 0:
            self.nbk.InsertPage(ichn, pnl, "byChannel", False)
        else:
            self.nbk.AddPage(pnl, "byChannel", False)

        self.spbc = wx.Panel(pnl)
        self.bxshr = wx.BoxSizer(wx.HORIZONTAL)

        bxsv = wx.BoxSizer(wx.VERTICAL)
# 1 x button by chn
        if self.nbRun == 1:
            self.chnlist = ['-- Channel --']
            px = self.nbRun
            for ichn in range(1, self.nbChn + 1):
                nchn = 'chn_%02d' % ichn
                self.chnlist.append(nchn)

        self.chxchn = wx.ComboBox(self.spbc, size=(
            125, 26), choices=self.chnlist, style=wx.CB_READONLY)
        self.chxchn.SetSelection(1)
        self.chxchn.Bind(wx.EVT_COMBOBOX, self.chnChoix)
        self.bxshr.Add(self.chxchn)

# 1 x button by pseudochn
        if self.nbRun == 1:
            self.pchnlist = ['-- Pseudo channel --']

        self.chxpchn = wx.ComboBox(self.spbc, size=(
            165, 26), choices=self.pchnlist, style=wx.CB_READONLY)
        self.chxpchn.SetSelection(0)
        self.chxpchn.Bind(wx.EVT_COMBOBOX, self.chnPChoix)
        self.bxshr.Add(self.chxpchn)

        self.bxshr.AddStretchSpacer()
# formula
        btnNF = wx.Button(self.spbc, id=-1, label='Go', size=(50, 30))
        btnNF.Bind(wx.EVT_BUTTON, self.addPChn)

        formule = '(chn_01+chn_02)/(chn_04-chn_03)'

        self.txtF = wx.TextCtrl(
            self.spbc, id=-1, value=formule, size=(350, 30))
        self.txtF.SetFont(
            wx.Font(12, wx.SWISS, wx.NORMAL, wx.BOLD, False, "Arial"))
        self.bxshr.Add(self.txtF)
        self.bxshr.Add(btnNF,  1)

        self.spbc.SetSizer(self.bxshr)
        self.spbc.Layout()

        bxsv.Add(self.spbc)

        if self.drwRefl == 0:
            self.cfig = figPlot0()
        elif self.disRefl == 0:
            self.cfig = figPlot0()
        else:
            self.cfig = figPlot()

        cnv = FigureCanvas(pnl, -1, self.cfig)
        cnv.mpl_connect('motion_notify_event', self.onMouseMotion)

        tlb = ToolBar(cnv)
        tlb.Realize()

        bxsv.Add(cnv,  -1, wx.EXPAND | wx.ALL, 5)
        bxsv.Add(tlb)

        pnl.SetSizer(bxsv)
        pnl.Layout()

    def addPChn(self, evt):
        npchn = self.txtF.GetValue()

# minimum control
        nexpr = npchn
        lri = []
        print(npchn)
        try:
            for ichn in range(1, self.nbChn + 1):
                nchn = 'chn_%02d' % ichn
                ri = random.uniform(1, 1000)
                while ri in lri:
                    ri = random.uniform(1, 1000)
                lri.append(ri)
                nexpr = nexpr.replace(nchn, '%s' % ri)
            eval(nexpr)
        except Exception:
            err = sys.exc_info()
            txt = 'WRONG FORMULA error: %s %s' % (err[0], err[1])
            print(txt)
            self.writeSB(txt, 'ORANGE', 10, 1)
            return
        else:
            if npchn in self.pchnlist:
                txt = 'Exists : %s' % npchn
                self.writeSB(txt, 'ORANGE', 10, 1)
                return
            else:
                txt = 'OK : %s' % npchn
                self.writeSB(txt, 'LIME GREEN', 10, 1)
                self.nbPChn += 1
                self.pchnlist.append(npchn)
                self.chxpchn.Append(npchn)
                self.chxpchn111.Append(npchn)
                self.chxchn.SetSelection(0)
                self.chxpchn.SetSelection(len(self.pchnlist) - 1)
                self.majtabyPChn()
                self.redrawPChn(len(self.pchnlist) - 1)

    def chnChoix(self, evt):
        sel = self.chxchn.GetCurrentSelection()
        self.redrawChn(sel)

    def chnPChoix(self, evt):
        sel = self.chxpchn.GetCurrentSelection()
        self.redrawPChn(sel)

    def redrawChn(self, i_chn):
        if i_chn == 0:
            return

        self.chxpchn.SetSelection(0)

        taxes = self.cfig.get_axes()
        for iplt in range(len(self.pltlist)):
            taxes[iplt] = self.majSubplot(taxes[iplt], 'chn', self.pltlist[
                                          iplt], i_chn, self.nbRun + 1)

        self.cfig.tight_layout()
        self.cfig.canvas.draw()

# find nearest float value
    def find_nearest(self, array, value):
        myarray = numpy.array(array)
        i = (numpy.abs(myarray - value)).argmin()
        v = array[i]

        if abs(min(v - value, value - v)) >= 10.0:
            i = -1

        return i

    def redrawPChn(self, i_pchn):
        if i_pchn == 0:
            return

        self.chxchn.SetSelection(0)

        taxes = self.cfig.get_axes()
        for iplt in range(len(self.pltlist)):
            taxes[iplt] = self.majSubplot(taxes[iplt], 'pchn', self.pltlist[
                                          iplt], i_pchn, self.nbRun + 1)

        self.cfig.tight_layout()
        self.cfig.canvas.draw()

# wavelengh : lamda = u'\u03BB' en  micrometre : mu = u'\u03BC'
# frequency : nu = '\u03BD' en GHz
    def onMouseMotion(self, event):
        """
                si wn < 400     : lambda = xxx micrometre
                sinon           : nu = yyy GHz

                delta min = u'\\u03B4'
        """
        if event.inaxes:
            ax = event.inaxes.title.get_text()
            x = event.xdata
            if x < 0:
                return

            try:

                txt0 = ""
                if re.search('formula', ax):
                    """ label formaule OnePlot different"""
                    bx = '%s %s )' % (ax.split(' ')[0], ax.split(' ')[1])
                    y = self.taby[bx][int(x) - 1]
                    txt = 'run #%02g Y=%g %s' % (
                        x, y, self.units_d[ax.split('_')[0]])
                elif re.search('chn', ax):
                    c = ax[-2:]
                    y = self.taby[ax][int(x) - 1]
                    if self.lstWv[int(c) - 1] >= 400.0:
                        txt0 = '%s=%.3f %sm]' % (
                            u'\u03BB',
                            (10000 / self.lstWv[int(c) - 1]), u'\u03BC')
                    else:
                        txt0 = '%s=%.4f GHz]' % (
                            u'\u03BD',
                            (spl * self.lstWv[int(c) - 1] / 1000000000))

                    txt = 'run #%02g Y=%g %s [wn=%.3f cm%s / ' % (
                        x, y, self.units_d[ax.split('_')[0]],
                        self.lstWv[int(c) - 1], exponentMinus1) + txt0
# run and prun case
                elif re.search('run', ax):
                    # run case
                    if ax.count('run') == 1:

                        if self.nbChn < 50 or self.isUVVIS:
                            nx = int(round(x)) - 1
                            y = self.taby[ax][nx]

                            txt = 'chn #%02g Y=%g %s [wn=%.3f cm%s / ' % (
                                nx + 1, y, self.units_d[ax.split('_')[0]],
                                self.lstWv[nx], exponentMinus1)

                        else:
                            nx = self.find_nearest(self.lstWv, x)
                            if nx < 0:
                                msgtxt = "No data"
                                self.writeSB(msgtxt, 'WHITE', 15, 0)
                                return
                            vx = self.lstWv[nx]
                            y = self.taby[ax][nx]
                            txt = '(chn #%02g) X=%.3f cm%s Y=%g %s [' % (
                                nx + 1, vx, exponentMinus1,
                                y, self.units_d[ax.split('_')[0]])

                        if self.lstWv[nx] >= 400.0:
                            txt0 = '%s=%.3f %sm]' % (
                                u'\u03BB',
                                (10000 / self.lstWv[nx]), u'\u03BC')
                        else:
                            txt0 = '%s=%.4f GHz]' % (
                                u'\u03BD',
                                (spl * self.lstWv[nx] / 1000000000))

                        msgtxt = txt + txt0

# prun case
                    elif ax.count('run') == 2:
                        if self.nbChn < 50 or self.isUVVIS:
                            nx = int(round(x)) - 1
                            y = self.taby[ax][nx]
                        else:
                            nx = self.find_nearest(self.lstWv, x)
                            if nx < 0:
                                txt = "No data"

                                self.writeSB(msgtxt, 'WHITE', 15, 0)
                                return

                        vx = self.lstWv[nx]

                        typ = '%s' % ax.split('_')[0]
                        ref = '%s_%s' % (typ, (ax.split(' ')[3]))
                        run = '%s_%s' % (typ, (ax.split(' ')[1]))

                        yf = self.taby[ref][nx]
                        yr = self.taby[run][nx]
                        form1 = '(chn #%02g) X=%.3f cm%s Y_%s=%g Y_minus=%g '
                        form2 = '(chn #%02g) X=%.3f cm%s Y_%s=%g Y_%s=%g '
                        if re.search('minus', ax):
                            txt = form1 % (
                                nx + 1, vx,
                                exponentMinus1, '%s' % ref[-2:], yf, yr - yf)
                        else:
                            txt = form2 % (
                                nx + 1, vx, exponentMinus1, '%s' % ref[-2:],
                                yf, '%s' % run[-2:], yr)

                        if self.lstWv[nx] >= 400.0:
                            txt0 = '[%s=%.3f %sm]' % (
                                u'\u03BB', (10000 / self.lstWv[nx]), u'\u03BC')
                        else:
                            txt0 = '[%s=%.4f GHz]' % (
                                u'\u03BD', (spl * self.lstWv[nx] / 1000000000))

                        msgtxt = txt + txt0

                else:
                    y = self.taby[ax][x - 1]
                    msgtxt = '%s : X=%02g Y=%g' % (ax.split('_')[0], x, y)

                self.writeSB(msgtxt, 'GREY', 15, 0)

            except Exception:
                return

    def MenuData(self):
        return (
            ("&File",  # File Menu
             ('&Quit', 'Quit', self.OnQuit, "quit", True)),
            ("&Help",  # Help Menu
             ("About", "About screen", self.OnAbout, "about", True),
             ("&Help", "Help", self.OnHelpHTML, "help", True)))

    def read_rad_h5(self, fradname, addsolar):
        """
        read radiance h5 file
        """
        self.nbRun += 1
        try:
            frad = h5py.File(fradname, 'r')
            txt = 'read file : %s [run %0d]' % (fradname, self.nbRun)
            self.writeSB(txt, 'GREEN', 5, 1)
        except Exception:
            txt = 'error access file : %s' % fradname
            self.writeSB(txt, 'RED', 10, 1)
            self.nbRun -= 1
            return

        h5misc = frad['/MISC/']
        misc = rttov.misc.Misc()
        misc.loadh5(h5misc)
        rsat = misc['SATELLITE']
        rins = misc['INSTRUMENT']
        self.nbChn = misc['NCHANNELS']

#
# one channel must be an array
#
        if self.nbChn < 2:
            rwave = numpy.array([misc['WAVENUMBERS']])
        else:
            rwave = misc['WAVENUMBERS']

        h5 = frad['/']
        if 'RADIANCE' in list(h5.keys()):
            h5rad = frad['/RADIANCE/']
            rad = rttov.radiance.Radiance()
            rad.loadh5(h5rad)
        if 'PCCOMP' in list(h5.keys()):
            h5pc = frad['/PCCOMP/']
            rad = rttov.pccomp.PCCOMP()
            rad.loadh5(h5pc)
            self.pccomp[self.nbRun] = 1

        # rad.display()

        frad.close()

# first read --> satellite and instrument
# or window reinitialisation
#

        if len(self.lstChn) == 0 or self.nbRun == 0:
            self.sat = rsat
            self.ins = rins
            self.txtsb = '%s / %s' % (
                self.sat.replace(' ', '').upper(), self.ins.upper())

            for i_chn in range(0, self.nbChn):
                n_chn = '%s_%d' % (self.ins, i_chn + 1)
                self.lstChn.append(n_chn)
                self.lstWv.append(rwave[i_chn])
        else:
            if rsat != self.sat or rins != self.ins:
                txt = 'Attn : %s/%s --> %s/%s' % (self.sat,
                                                  self.ins, rsat, rins)
                self.writeSB(txt, 'RED', 10, 1)
                self.nbRun -= 1
                return

        if self.nbRun == 1:
            if self.pccomp[1]:
                self.ttotal = numpy.array([rad['TOTAL_PCCOMP']])
                self.tbt = numpy.array([rad['BT_PCCOMP']])

                self.tclear = numpy.zeros((1, self.nbChn))
                self.tbtclear = numpy.zeros((1, self.nbChn))
                self.trefl = numpy.zeros((1, self.nbChn))
                self.treflclear = numpy.zeros((1, self.nbChn))
            else:
                if self.nbChn < 2:
                    self.ttotal = numpy.array([[rad['TOTAL']]])
                    self.tclear = numpy.array([[rad['CLEAR']]])
                    self.tbt = numpy.array([[rad['BT']]])
                    self.tbtclear = numpy.array([[rad['BT_CLEAR']]])
                    self.trefl = numpy.array([[rad['REFL']]])
                    self.treflclear = numpy.array([[rad['REFL_CLEAR']]])
                else:
                    self.ttotal = numpy.array([rad['TOTAL']])
                    self.tclear = numpy.array([rad['CLEAR']])
                    self.tbt = numpy.array([rad['BT']])
                    self.tbtclear = numpy.array([rad['BT_CLEAR']])
                    self.trefl = numpy.array([rad['REFL']])
                    self.treflclear = numpy.array([rad['REFL_CLEAR']])
        else:
            if self.pccomp[self.nbRun]:
                self.ttotal = numpy.concatenate(
                    (self.ttotal, [rad['TOTAL_PCCOMP']]))
                self.tbt = numpy.concatenate((self.tbt, [rad['BT_PCCOMP']]))
                self.tclear = numpy.concatenate(
                    (self.tclear, numpy.zeros((1, self.nbChn))))
                self.tbtclear = numpy.concatenate(
                    (self.tbtclear, numpy.zeros((1, self.nbChn))))
                self.trefl = numpy.concatenate(
                    (self.trefl, numpy.zeros((1, self.nbChn))))
                self.treflclear = numpy.concatenate(
                    (self.treflclear, numpy.zeros((1, self.nbChn))))
            else:
                if self.nbChn < 2:
                    self.ttotal = numpy.concatenate(
                        (self.ttotal, [[rad['TOTAL']]]))
                    self.tclear = numpy.concatenate(
                        (self.tclear, [[rad['CLEAR']]]))
                    self.tbt = numpy.concatenate((self.tbt, [[rad['BT']]]))
                    self.tbtclear = numpy.concatenate(
                        (self.tbtclear, [[rad['BT_CLEAR']]]))
                    self.trefl = numpy.concatenate(
                        (self.trefl, [[rad['REFL']]]))
                    self.treflclear = numpy.concatenate(
                        (self.treflclear, [[rad['REFL_CLEAR']]]))
                else:
                    self.ttotal = numpy.concatenate(
                        (self.ttotal, [rad['TOTAL']]))
                    self.tclear = numpy.concatenate(
                        (self.tclear, [rad['CLEAR']]))
                    self.tbt = numpy.concatenate((self.tbt, [rad['BT']]))
                    self.tbtclear = numpy.concatenate(
                        (self.tbtclear, [rad['BT_CLEAR']]))
                    self.trefl = numpy.concatenate((self.trefl, [rad['REFL']]))
                    self.treflclear = numpy.concatenate(
                        (self.treflclear, [rad['REFL_CLEAR']]))

        self.majtabyChn()
        self.majtabyPChn()

        if min(rwave) > 3333.0:  # update to 3333 cm-1  solar only channels
            self.isUVVIS = True

        if self.isUVVIS:
            self.pltlist = self.pltlistuv
            txt = "uv/vis, no BT"
            self.writeSB(txt, 'YELLOW', 10, 1)
            self.solar = 1
# no Refl for micro waves : wn < 400
        elif max(rwave) < 400.0:
            self.drwRefl = 0
            self.pltlist = self.pltlistir
            txt = "microwaves, no Refl"
            self.writeSB(txt, 'YELLOW', 10, 1)
        else:
            # Refl exists but hidden if addsolar=0
            if addsolar == 0 and self.solar == 0:
                txt = "addSolar=False. Refl hidden."
                self.writeSB(txt, 'YELLOW', 10, 1)
                self.disRefl = 0
                self.pltlist = self.pltlistir
            elif addsolar == 1 and self.solar == 0:
                self.solar = 1
                txt = "addSolar=True. Refl shown."
                self.writeSB(txt, 'YELLOW', 10, 1)
                self.disRefl = 1
                self.pltlist = self.pltlistall
# update combobox 111 choice if necessaru
                if self.nbRun > 1:
                    # reinit byRun and byChn : need page index to keep order
                    nbk_run = -1
                    nbk_chn = -1
                    for i in range(self.nbk.GetPageCount()):
                        if self.nbk.GetPageText(i) == "byRun":
                            nbk_run = i
                        elif self.nbk.GetPageText(i) == "byChannel":
                            nbk_chn = i

                    if self.nbChn < 50:
                        self.byChn = 1
                        self.mz = 5
                        self.nbk.DeletePage(nbk_chn)
                        self.initPlotChns(nbk_chn)

                    self.nbk.DeletePage(nbk_run)
                    self.initPlotRuns(nbk_run)

                    if self.nbChn < 50:
                        self.redrawChn(1)
                        self.redrawPChn(0)

                    if self.chxplt111.GetCount() == 4:
                        self.chxplt111.Append('REFL')
                        self.chxplt111.Append('REFL_CLEAR')

# end read : initplot ou addplot
        if self.nbRun == 1:
            # if nbChn > 50 :  byrun only
            if self.nbChn < 50:
                self.byChn = 1
                self.mz = 5
                self.initPlotChns(-1)

            self.refRun = 1
            self.initPlotRuns(-1)
            self.initPlot111()
            self.redraw111(0, 0, -1, -1, -1)

# run_01 = refRun
            self.chxrefrun.SetSelection(0)
            self.chxrefrun111.SetSelection(0)

# end init
            self.fin = time.time()
            delta = self.fin - self.deb
            self.txt = 'End init : %f sec.' % delta
            self.writeSB(self.txt, 'LIME GREEN', 5, 1)

            if self.nbChn < 50:
                self.redrawChn(1)
                self.redrawPChn(0)
        else:
            self.addRuns()
            self.chxprun111.SetSelection(0)
            self.redraw111(self.chxplt111.GetSelection(),
                           self.nbRun - 1, -1, -1, -1)

            if self.nbChn < 50:
                self.redrawChn(self.chxchn.GetSelection())
                self.redrawPChn(self.chxpchn.GetSelection())
                self.chxchn111.SetSelection(0)
                self.chxpchn111.SetSelection(0)

        self.redrawRun(self.nbRun)

    def majtabyChn(self):
        # update taby channels
        for ichn in range(1, self.nbChn + 1):
            self.taby['TOTAL_chn_%02d' % ichn] = numpy.transpose(self.ttotal)[
                ichn - 1][:]
            self.taby['CLEAR_chn_%02d' % ichn] = numpy.transpose(self.tclear)[
                ichn - 1][:]
            self.taby['BT_chn_%02d' % ichn] = numpy.transpose(self.tbt)[
                ichn - 1][:]
            self.taby['BT_CLEAR_chn_%02d' % ichn] = numpy.transpose(
                self.tbtclear)[ichn - 1][:]
            self.taby['REFL_chn_%02d' % ichn] = numpy.transpose(self.trefl)[
                ichn - 1][:]
            self.taby['REFL_CLEAR_chn_%02d' % ichn] = numpy.transpose(
                self.treflclear)[ichn - 1][:]

    def majtabyPChn(self):
        # update taby pseudo-channels

        for ipchn in range(1, self.nbPChn + 1):
            npchn = self.pchnlist[ipchn]
            self.taby['TOTAL_( formula_%02d )' % ipchn] = []
            self.taby['CLEAR_( formula_%02d )' % ipchn] = []
            self.taby['BT_( formula_%02d )' % ipchn] = []
            self.taby['BT_CLEAR_( formula_%02d )' % ipchn] = []
            self.taby['REFL_( formula_%02d )' % ipchn] = []
            self.taby['REFL_CLEAR_( formula_%02d )' % ipchn] = []

            for irun in range(1, self.nbRun + 1):
                ttotaleval = npchn
                tcleareval = npchn
                tbteval = npchn
                tbtcleareval = npchn
                trefleval = npchn
                treflcleareval = npchn
                for ichn in range(1, self.nbChn + 1):
                    nchn = self.chnlist[ichn]
                    if re.search(nchn, ttotaleval):
                        ittotal = 'self.ttotal[%d][%d]' % (irun - 1, ichn - 1)
                        ttotaleval = ttotaleval.replace(nchn, ittotal)
                        itclear = 'self.tclear[%d][%d]' % (irun - 1, ichn - 1)
                        tcleareval = tcleareval.replace(nchn, itclear)
                        itbt = 'self.tbt[%d][%d]' % (irun - 1, ichn - 1)
                        tbteval = tbteval.replace(nchn, itbt)
                        itbtclear = 'self.tbtclear[%d][%d]' % (
                            irun - 1, ichn - 1)
                        tbtcleareval = tbtcleareval.replace(nchn, itbtclear)
                        itrefl = 'self.trefl[%d][%d]' % (irun - 1, ichn - 1)
                        trefleval = trefleval.replace(nchn, itrefl)
                        itreflclear = 'self.treflclear[%d][%d]' % (
                            irun - 1, ichn - 1)
                        treflcleareval = treflcleareval.replace(
                            nchn, itreflclear)

                self.taby['TOTAL_( formula_%02d )' %
                          ipchn].append(eval(ttotaleval))
                self.taby['CLEAR_( formula_%02d )' %
                          ipchn].append(eval(tcleareval))

                self.taby['BT_( formula_%02d )' % ipchn].append(eval(tbteval))
                self.taby['BT_CLEAR_( formula_%02d )' %
                          ipchn].append(eval(tbtcleareval))

                self.taby['REFL_( formula_%02d )' %
                          ipchn].append(eval(trefleval))
                self.taby['REFL_CLEAR_( formula_%02d )' %
                          ipchn].append(eval(treflcleareval))


if __name__ == "__main__":
    print("******************************************************************")
    print("*******   RadianceFrame environment   ****************************")
    print("******************************************************************")
    print('Executing on ', os.uname())
    print('Python version ', sys.version)
    print('wxPython version ', wx.__version__)
    print('matplotlib version ', matplotlib.__version__)
    print("******************************************************************")

    fh5 = []

    for i in range(len(sys.argv)):
        fh5.append(sys.argv[i])

    print("f=", fh5)
    if len(fh5) < 2:
        from util import rttov_gui_data_test_dir
        data_test_dir = rttov_gui_data_test_dir()
        radfile = os.path.join(data_test_dir, "radr.h5")
    else:
        radfile = fh5[1]
    print("radiance file:", radfile)
    app = wx.App(0)
    frame = RadianceFrame(None, "Radiance Viewer", radfile, 0)

    app.MainLoop()
