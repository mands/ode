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
  mpl.use("pgf")
  pgf_with_rc_fonts = {
      "font.family": "serif", # use serif/main font for text elements
      "text.usetex": True,    # use inline math for ticks
      "pgf.rcfonts": False,   # don't setup fonts from rc parameters
      "pgf.texsystem": "lualatex",
#      "font.serif": ["TeX Gyre Termes"],                   # use latex default serif font
#     "font.sans-serif": ["TeX Gyre Heros"], # use a specific sans-serif font
#      "font.monospace": ["TeX Gyre Cursor"], # use a specific sans-serif font
  }
  mpl.rcParams.update(pgf_with_rc_fonts)

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


"""Reset a plot - wxh in inches, scale=1"""
def reset_plot(w = 6, h=3.25, s=1):
  # h = 3.5 ?
  plt.close('all')
  plt.figure(figsize=(w*s,h*s)) #figsize=(4.5,2.5))
  plt.subplot(1,1,1)
  plt.tight_layout(.5)

"""Save the file or plot it"""
def save_show(name):
  if args.tex:
    plt.savefig(name, format='pgf')
  else:
    plt.grid(True)
    plt.show()

"""Plot a line graph"""
def plot_data(filename, col, color=None, start=1000, stop=1500, period=1, shift=0):
  logging.debug("Setting up a plot for col {} from {}".format(col, filename))
  (data, num_cols) = openFile(filename)
  # plot the data
  # get and plot the specified columns
  mod_start = start/period
  mod_stop = stop/period
  mod_shift = shift/period
  time_data = data[mod_start:mod_stop,0]
  res_data = data[mod_start+mod_shift:mod_stop+mod_shift,col]
  if color is None:
    plt.plot(time_data, res_data)
  else:
    plt.plot(time_data, res_data, color=color)


"""Calc margins for bar graph"""
def calc_margins(entries, elements=1):
  ind = np.arange(entries)
  margins = 0.25
  width = (1 - margins) / elements
  ind_shift = ind + (margins / 2)
  x_cen = ind + 0.5
  return (ind_shift, width, x_cen)



