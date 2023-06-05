# -*- coding: utf-8 -*-

from rview import util
try:
    import wx.grid
except ImportError:
    import sys
    sys.stderr.write('ERROR: wxPython is not installed\n')
    sys.exit(1)

import rmodel
import copy


class DataTable(wx.grid.GridTableBase):
    """ interface between the model and the view. Implements PyGridTableBase"""

    def __init__(self, data, dataName='EMIS'):
        """ init the data which could be either EMIS DATA ou REFL DATA """
        print("init DataTable", data)
        self.myData = copy.deepcopy(data)
        self.dataName = dataName
        super().__init__()
        if (dataName == "EMIS"):
            self.colsNames = {0: dataName + "_IN",
                              1: dataName + "_OUT",
                              2: "CALC" + dataName,
                              3: "SPECULARITY"}
        else:
            self.colsNames = {0: dataName + "_IN",
                              1: dataName + "_OUT",
                              2: "CALC" + dataName,
                              3: "DIFFUSE_REFL_IN",
                              4: "DIFFUSE_REFL_OUT",
                              5: "REFL_CLOUD_TOP"}

    def GetAttr(self, row, col, kind):
        attr = wx.grid.GridCellAttr()
        if col == 2:
            # attr.SetRenderer(wx.grid.GridCellStringRenderer())
            pass
        elif col in (1, 4, 5):
            attr.SetReadOnly()
            attr.SetBackgroundColour("light grey")
        else:
            attr.SetRenderer(wx.grid.GridCellFloatRenderer())
        return attr

    def GetNumberRows(self):
        name = self.dataName + '_IN'
        return 1 if (
            len(self.myData[name].shape) == 0) else self.myData[name].shape[0]

    def GetNumberCols(self):
        return 4 if (self.dataName == "EMIS") else 6

    def IsEmptyCell(self, row, col):
        return False

    def GetValue(self, row, col):
        return self.myData[self.colsNames[col]][row]

    def SetValue(self, row, col, value):
        print("setvalue for row", row, "col:", col, "value:", value)
        if col == 2:
            print("SetValue", col, row, value)
            myvalue = value in (1, "1", "", "True", True, "T", "true", "t")
            self.myData[self.colsNames[col]][row] = myvalue
        else:
            # ensure value between 0 and 1
            oldvalue = self.myData[self.colsNames[col]][row]
            try:
                myvalue = float(value)
            except Exception:
                myvalue = oldvalue
            myvalue = min(myvalue, 1.)
            myvalue = max(myvalue, 0.)
        print("set data : ", myvalue)
        self.myData[self.colsNames[col]][row] = myvalue

    def GetColLabelValue(self, col):
        return self.colsNames[col]

    def UpdateData(self, data):
        self.myData = data


class GridView(util.GenericView):
    """ grid editor of the application for reflectance / emissivity """

    helpMessage = """

    This window allows you to modify reflectance or emissivity input values.
    Tip for check boxes : write 1 for check off , write 0 or nothing otherwise.

    """

    def __init__(self, parent, title, data, name):
        print("init GridView")
        self.myData = data
        super().__init__(parent, title)
        sizer = wx.BoxSizer(wx.HORIZONTAL)

        self.CreateMenuBar()
        self.SetMinSize((500, 300))
        self.SetTitle(title)
        self.grid = wx.grid.Grid(self, -1)
        self.table = DataTable(data, name)
        self.grid.SetTable(self.table, True)
        self.grid.AutoSize()
        self.nbRow = self.table.GetNumberRows()
        sizer.Add(self.grid, 1, wx.EXPAND)
        self.SetSizer(sizer)
        if self.table.GetNumberRows() <= 25:
            sizer.Layout()
            self.Fit()
        self.sb = self.CreateStatusBar()
        self.Centre()
        self.Show(True)

    def OnUpdateData(self, data, name):
        self.write("surfedit Update Data for " + name)

        if len(data[name + '_IN'].shape) == 0:
            newNbRow = 1
        else:
            newNbRow = data[name + '_IN'].shape[0]

        if newNbRow != self.nbRow:
            self.write(
                "surfedit nbRow has changed must make new DataTable for " +
                name)
            self.table = DataTable(data, name)
            self.grid.SetTable(self.table, True)
            self.nbRow = self.table.GetNumberRows()
        else:
            self.table.UpdateData(data)

        self.Refresh()

    def OnDoubleClick(self, e):
        print("doubleClick")

    def OnApplyChange(self, e):
        self.myData = self.table.myData
        return self.myData

    def OnSaveSurface(self, e): pass

    def OnSaveSurfaceAs(self, e): pass

    def MenuData(self):
        """ define the data for the menu
        """
        return(("&File",  # File Menu

                ("Apply change", "Apply change for the surface ",
                 self.OnApplyChange, "applyChange", True),
                ('&Quit', 'Quit', self.OnQuit, "quit", True)),
               ("&Help",  # Help Menu
                ("About", "About screen", self.OnAbout, "about", True),
                ("&Help", "Help", self.OnHelp, "help", True)))


if __name__ == "__main__":
    p = rmodel.project.Project()
    prefix = p.config.ENV['RTTOV_GUI_PREFIX']
    print("Configuration : ", dir)

    err = p.openProfile(p.config.ENV["RTTOV_GUI_PROFILE_DIR"] +
                        "/cldaer101lev_allgas.H5", 1)
    if err != 0:
        print("error open profile")
        sys.exit(1)
    p.myCoeffs.fileName["standard"] = (p.config.ENV["RTTOV_GUI_COEFF_DIR"] +
                                       "/rttov9pred54L/rtcoef_eos_2_modis.dat")
    p.loadCoefficients()
    print(p.myCoeffs.nchannels)
    p.myOption["ADDSOLAR"].value = True
    p.runDirect()

    emis = p.myEmissivity
    print(type(emis))
    print(emis.keys())
    print(emis)
    print("--emis---------")
    print(emis)
    print("-----------------")
    ex = wx.App()
    gv = GridView(None, "Emissivity", emis, 'EMIS')

    ex.MainLoop()
