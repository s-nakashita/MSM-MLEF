# -*- coding: utf-8 -*-
import wx
import rmodel
from rview import util
import wx.lib.agw.flatnotebook as fnb
import wx.lib.agw.floatspin as FS
import os
import logging


class OptionView(util.GenericView):
    """ Option Dialog window of the application """
    helpPage = os.environ["RTTOV_GUI_PREFIX"] + "/doc/helpOptions.html"
    helpTitle = "Options help"

    def __init__(self, parent, title, project):
        self.project = project
        self.myOptions = project.myOption  # linked objects
        # define the optionsThemes according to the coefficient files
        # general conf 0, general RT 1, VIS/IR 2, PC 3, MW 4 , Interl 5
        themes = self.myOptions.optionsThemes
        if self.project.myCoeffs.isMW():
            self.optionsThemes = themes[:2] + themes[4:]
        elif self.project.myCoeffs.hasPC():
            self.optionsThemes = themes[:4] + themes[5:]
        else:
            self.optionsThemes = themes[:3] + themes[5:]

        # for each theme keep the list of options
        self.optionsThemesList = {theme: self.myOptions.optionsThemesList[
            theme] for theme in self.optionsThemes}

        super().__init__(parent,  title)
        self.CreateMenuBar()

        self.SetMinSize((880, 700))
        self.panel1 = self
        self.masterSizer = wx.BoxSizer(wx.VERTICAL)
        self.notebook = fnb.FlatNotebook(self.panel1, -1,
                                         agwStyle=(fnb.FNB_NO_X_BUTTON |
                                                   fnb.FNB_FANCY_TABS))
        self.cb = {}
        self.combo = {}
        self.numberParam = {}
        self.numberParamDict = {"RAYLEIGH_MAX_WAVELENGTH": {"step": 0.5,
                                                            "format": "%f",
                                                            "ndigits": 2},
                                "RAYLEIGH_MIN_PRESSURE": {"step": 1,
                                                          "format": "%f",
                                                          "ndigits": 1},
                                "CLDCOL_THRESHOLD": {"step": 0.001,
                                                     "format": "%f",
                                                     "ndigits": 3},
                                "DOM_ACCURACY": {"step": 0.001,
                                                 "format": "%f",
                                                 "ndigits": 3},
                                "DOM_OPDEP_THRESHOLD": {"step": 0.1,
                                                        "format": "%f",
                                                        "ndigits": 2},
                                "DOM_NSTREAMS": {"step": 2,
                                                 "format": "%f",
                                                 "ndigits": 0},
                                "NPCSCORES": {"step": 1,
                                              "format": "%f",
                                              "ndigits": 0}}

        for i in range(len(self.optionsThemes)):
            caption = self.optionsThemes[i]
            self.notebook.AddPage(self.CreatePage(self.notebook, i), caption)

        # hide irelevant page
        #

        self.masterSizer.Add(self.notebook, 1, wx.ALL | wx.EXPAND, 5)
        self.masterSizer.Add((10, 10), flag=wx.EXPAND)

        self._CreateButtons()

        self.masterSizer.Add(self.btnSizer, flag=wx.ALIGN_CENTER, border=10)
        self.masterSizer.Add((10, 10), flag=wx.EXPAND)
        self.SetValueItems()
        self.sb = self.CreateStatusBar()

        self.Bind(wx.EVT_CLOSE, self.OnClose)
        for param in ['IPCBND', 'FASTEM_VERSION', 'IR_SCATT_MODEL',
                      'VIS_SCATT_MODEL']:
            if param in self.combo:
                self.combo[param].Bind(wx.EVT_COMBOBOX, self.UpdateOptions)
        for param in ['ADDPC', 'DO_LAMBERTIAN', 'ADDRADREC', 'ADDCLOUDS',
                      'ADDAEROSL', 'ADDSOLAR']:
            if param in self.cb:
                self.cb[param].Bind(wx.EVT_CHECKBOX, self.UpdateOptions)

        self.UpdateOptions(None)

        self.SetSizer(self.masterSizer)
        self.Fit()
        self.Layout()
        self.Centre()
        self.Show(True)

    def CreatePage(self, notebook, option_theme_number_in_list):
        '''
        Creates a :class:`Panel` containing options by theme.
        :param `notebook`: an instance of `FlatNotebook`;
        :param `option_theme_number_in_list`:
        '''
        p = wx.Panel(notebook)
        p.SetBackgroundColour(wx.Colour(217, 217, 214))
        i = option_theme_number_in_list
        gbs = wx.GridBagSizer(5, 4)
        line = 0
        col = 1
        for parametre in self.optionsThemesList[self.optionsThemes[i]]:
            if not self.myOptions[parametre].hidden:
                line = line + 1
                if line > 9:
                    line = 1
                    col = 8
                gbs.Add(wx.StaticText(
                        p, -1, parametre.swapcase()),
                        pos=(line, col),
                        span=(1, 3),
                        flag=wx.RIGHT | wx.ALIGN_CENTER_VERTICAL,
                        border=5)
                positem = (line, col + 3)
                if self.myOptions[parametre].otype == dict:
                    odict = self.myOptions[parametre].odict
                    self.combo[parametre] = wx.ComboBox(
                        p,
                        choices=list(odict.values()))
                    gbs.Add(self.combo[parametre], border=5,
                            pos=positem,
                            span=(1, 3),
                            flag=wx.EXPAND)
                elif (parametre in list(self.numberParamDict.keys())):
                    self.numberParam[parametre] = FS.FloatSpin(
                        p, -1,
                        min_val=self.myOptions[
                            parametre].min_max_values[0],
                        max_val=self.myOptions[
                            parametre].min_max_values[1],
                        increment=self.numberParamDict[parametre][
                            "step"],
                        agwStyle=FS.FS_LEFT)
                    self.numberParam[parametre].SetFormat(
                        self.numberParamDict[parametre]["format"])
                    self.numberParam[parametre].SetDigits(
                        self.numberParamDict[parametre]["ndigits"])

                    gbs.Add(self.numberParam[parametre],
                            pos=positem,
                            span=(1, 3),
                            border=5,
                            flag=wx.EXPAND)
                else:
                    self.cb[parametre] = wx.CheckBox(
                        p, label="")
                    gbs.Add(self.cb[parametre],
                            pos=positem,
                            span=(1, 1),
                            flag=wx.LEFT | wx.TOP | wx.EXPAND,
                            border=5)

        p.SetSizerAndFit(gbs)
        return p

    def UpdateOptions(self, e):
        """ check options values versus status and update the vue """
        self.GetValuesItems()
        self.project.ctrlCoherence()
        self.SetValueItems()

    def _MakeBinding(self):
        """ set the trivial Binding for the View """
        self.Bind(wx.EVT_BUTTON, self.OnCancel, self.cancelBtn)

    def OnItemFocus(self, e):  # TODO not working
        for (i, value) in list(self.cb.items()):
            if (value == e.GetEventObject()):
                self.sb.PushStatusText(
                    self.myOptions[i].comment, 1)

    def _CreateButtons(self):

        self.btnSizer = wx.BoxSizer(wx.HORIZONTAL)
        self.cancelBtn = wx.Button(self.panel1, wx.ID_CANCEL, label="Revert")
        self.cancelBtn.SetHelpText("Revert to previous options")
        self.btnSizer.Add(
            self.cancelBtn, flag=wx.RIGHT, border=10)

        self.applyBtn = wx.Button(self.panel1, wx.ID_OK, label="Apply")
        self.applyBtn.SetHelpText("Apply options")
        self.applyBtn.SetDefault()
        self.btnSizer.Add(self.applyBtn, flag=wx.RIGHT, border=10)

        # binding cancel button
        self.cancelBtn.Bind(wx.EVT_BUTTON, self.OnCancel)

    def SetValueItems(self):
        for i in self.myOptions.options_list_logical:
            if not self.myOptions[i].hidden and i in self.cb:
                self.cb[i].Enable(self.myOptions[i].status)
                self.cb[i].SetValue(self.myOptions[i].value)

        for param in list(self.combo.keys()):
            if not self.myOptions[param].hidden:
                odict = self.myOptions[param].odict
                logging.debug("SetValueItems " + param +
                              str(self.myOptions[param].value) +
                              str(list(odict.values())))

                myval = self.myOptions[param].getValue()
                logging.debug("SetValueItems " + param +
                              str(odict) + " myval " + str(myval))
                if param in self.combo:
                    self.combo[param].Enable(self.myOptions[param].status)
                    self.combo[param].SetItems(list(odict.values()))
                    self.combo[param].SetValue(myval)

        for param in list(self.numberParamDict.keys()):
            if not self.myOptions[param].hidden and param in self.numberParam:
                self.numberParam[param].SetValue(self.myOptions[param].value)
                self.numberParam[param].Enable(self.myOptions[param].status)

    def SetOptions(self, options):
        self.myOptions = options
        self.SetValueItems()

    def GetOptions(self):
        return self.myOptions

    def GetValuesItems(self):
        for i in self.myOptions.options_list_logical:
            if not self.myOptions[i].hidden and i in self.cb:
                self.myOptions[i].value = self.cb[i].GetValue()
        for i in list(self.combo.keys()):
            if not self.myOptions[i].hidden:
                value = self.combo[i].GetValue()
                odict = self.myOptions[i].odict
                self.myOptions[i].value = [k for k, v in odict.items()
                                           if v == value][0]
                logging.debug("value" + str(value) +
                              " result " + str(self.myOptions[i].value))

        try:
            for parameter in ['CLDCOL_THRESHOLD',
                              "DOM_OPDEP_THRESHOLD",
                              "DOM_ACCURACY",
                              "RAYLEIGH_MAX_WAVELENGTH",
                              "RAYLEIGH_MIN_PRESSURE"]:
                if parameter in self.numberParam:
                    self.myOptions[parameter].value = float(
                        self.numberParam[parameter].GetValue())
            for parameter in ['DOM_NSTREAMS',
                              'NPCSCORES']:
                if parameter in self.numberParam:
                    self.myOptions[parameter].value = int(
                        self.numberParam[parameter].GetValue())

        except ValueError:
            self.ShowErrorMessageDialogBox(
                var=parameter, type='number')

    def ShowErrorMessageDialogBox(self, varName, varType):
        message = "variable " + varName + " must be of " + varType + " type"
        dlg = wx.MessageDialog(
            None, message, caption="Error", style=wx.ICON_ERROR)
        dlg.ShowModal()
        dlg.DeletePendingEvents()
        wx.CallAfter(dlg.Destroy)

    def OnCancel(self, e):
        """ Cancel modifications made in the frame
            set all widget with initial option values"""
        self.SetValueItems()

    def OnApply(self, e):
        """ take values from the windows and save it in options"""
        self.GetValuesItems()

    def OnSave(self, e):
        """ take value from the windows and save it in options"""
        self.GetValuesItems()

    def _initItem(self):
        self.applyItem = 0
        self.saveItem = 0

    def MenuData(self):
        """ define the data for the menu
        """
        return(("&File",  # File Menu
                ("Apply options", "Apply the options",
                 self.OnApply, "applyOptions", True),
                ("Save options",
                 "Apply and Save the options and the profile in a file",
                 self.OnSave, "saveOptions", True),
                ("", "", "", "", True),
                ('&Quit', 'Quit', self.OnQuit, "quit", True)),
               ("&Help",  # Help Menu
                ("About", "About screen", self.OnAbout, "about", True),
                ("&Help", "Help", self.OnHelpHTML, "help", True)))


if __name__ == "__main__":
    p = rmodel.project.Project()
    p.openProfile(p.config.ENV["RTTOV_GUI_PROFILE_DIR"] +
                  '/cldaer101lev_allgas.H5')

    for option in p.myOption.options_list:
        p.myOption[option].status = True

    coefFile = p.config.ENV['RTTOV_GUI_COEFF_DIR'] + \
        "/rttov9pred101L/rtcoef_metop_2_iasi.H5"
    pcFile = p.config.ENV['RTTOV_GUI_COEFF_DIR'] + \
        "/pc/pccoef_metop_2_iasi_landsea_trace_aer.H5"
    p.myCoeffs.fileName["standard"] = coefFile
    p.loadCoefficients()
    ex = wx.App()
    p.myOption.display()
    mv = OptionView(None, "Options", p)
    print(mv.optionsThemes)

    ex.MainLoop()
