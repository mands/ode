### Latex Plotting/Matplotlib helpers
################################################    

# parse initial args
import argparse
parser = argparse.ArgumentParser()
parser.add_argument('-t', '--tex', action='store_true')
args = parser.parse_args()

# setup tex output
if args.tex:
  import matplotlib as mpl
  #from matplotlib.backends.backend_pgf import FigureCanvasPgf
  #mpl.backend_bases.register_backend('pdf', FigureCanvasPgf)
  mpl.use("pgf")
  pgf_with_pdflatex = {
      "pgf.texsystem": "lualatex",
      "text.usetex": True,    # use inline math for ticks
      "pgf.rcfonts": False,   # don't setup fonts from rc parameters
      "font.family": "serif", # use serif/main font for text elements
#      "font.serif": ["TeX Gyre Termes"],                   # use latex default serif font
#      "font.sans-serif": ["TeX Gyre Heros"], # use a specific sans-serif font
#      "font.monospace": ["TeX Gyre Cursor"], # use a specific sans-serif font
    "pgf.preamble": [
      r"\usepackage[math-style=TeX]{unicode-math}",
      #r"\usepackage[protrusion=true, expansion=true]{microtype}",
      r"\setmathfont{TeX Gyre Termes Math}",
      r"\setmainfont{TeX Gyre Termes}",     # manually setting the main font
      #r"\setsansfont{TeX Gyre Heros}",
      #r"\setmonofont{TeX Gyre Cursor}",
    ]
  }
  mpl.rcParams.update(pgf_with_pdflatex)

# Main imports
import logging
logging.basicConfig(level=logging.DEBUG)
from Common import openFile, plot, getCols
import matplotlib.pyplot as plt
import numpy as np

# GLOBALS
col1 = '#268bd2'
col2 = '#dc322f'
col3 = '#d33682'
max_x_bbox=1.0625
max_y_bbox=1.1125

"""Reset a plot - wxh in inches, scale=1"""
def reset_plot(w=6.3, h=2.75, s=1):
  # h = 3 or 2.75 ?
  plt.close('all')
  plt.figure(figsize=(w*s,h*s)) #figsize=(4.5,2.5))
  ax = plt.subplot(1,1,1)
  #plt.tight_layout(.5)
  return ax

"""Save the file or plot it"""
def save_show(name, show_grid=True):
  plt.grid(show_grid)
  plt.tight_layout(.5)
  if args.tex:
    plt.savefig(name, format='pdf')
  else:
    #plt.title(name)
    plt.show()

#"""Plot a line graph"""
#def plot_data(filename, col, color=None, start=1000, stop=1500, period=1, shift=0):
#  logging.debug("Setting up a plot for col {} from {}".format(col, filename))
#  (data, num_cols) = openFile(filename)
#  # plot the data
#  # get and plot the specified columns
#  mod_start = start/period
#  mod_stop = stop/period
#  mod_shift = shift/period
#  time_data = data[mod_start:mod_stop,0]
#  res_data = data[mod_start+mod_shift:mod_stop+mod_shift,col]
#  if color is None:
#    plt.plot(time_data, res_data)
#  else:
#    plt.plot(time_data, res_data, color=color)


"""Plot a line graph"""
def plot_data(filename, col, color=None, start=1000, stop=1500, period=1, shift=0, linestyle=None):
  logging.debug("Setting up a plot for col {} from {}".format(col, filename))
  (data, num_cols) = openFile(filename)
  # plot the data
  # get and plot the specified columns
  mod_start = start/period
  mod_stop = stop/period
  mod_shift = shift/period
  time_data = data[mod_start:mod_stop,0]
  res_data = data[mod_start+mod_shift:mod_stop+mod_shift,col]
  if color is None and linestyle is None:
    plt.plot(time_data, res_data)
  elif linestyle is None:
    plt.plot(time_data, res_data, color=color)
  else:
    plt.plot(time_data, res_data, color=color, linestyle=linestyle)


"""Plotting helper functions for chaing together operations"""
def sum_res(cols, data):
  return (data[:,0], np.sum(data[:,cols], axis=1))

def open_res(col, data):
  return (data[:,0], data[:,col])

#def multi_cols(cols, data):
#  return (data[:,0], data[:,cols])

def ssa_res(div_total, data_f, data):
  time_d, res_d = data_f(data)
  return (time_d, res_d / div_total)

def sing_plot(filename, data_f):
  (data, _) = openFile(filename)
  (time_d, res_d) = data_f(data)
  plt.plot(time_d, res_d)



"""Calc margins for bar graph"""
def calc_margins(entries, elements=1):
  ind = np.arange(entries)
  margins = 0.25
  width = (1 - margins) / elements
  ind_shift = ind + (margins / 2)
  x_cen = ind + 0.5
  return (ind_shift, width, x_cen)



