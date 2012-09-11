/* Hodgkin Model
** A simple ODE model for benchmarking purposes
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "odelibrary.h"

// func declarations
void euler_test(double start, double stop, double step, double sample, const char* out_file);

int main(void) {

  double start = 0; 
  double stop = 10; 
  //double step = 0.0000005; 
  //double sample = 2000000;
  
  double step = 0.00001; 
  double sample = 1;  
  const char* filename = "euler_test_c.bin";

  init();
  euler_test(start, stop, step, sample, filename);
  shutdown();

  exit(EXIT_SUCCESS);
}


void euler_test(double start, double stop, double step, double sample, const char* out_file) {
  // setup initial values
  double y = 2;

  // euler loop params
  double cur_sample = sample;
  double max_stop = stop + (step/2);

  start_sim(out_file);

  // main forward euler loop - to max_stop ?
  for (double cur_param = start; cur_param < max_stop; cur_param += step) {
    // setup deltas
    double dY = y - cur_param;

    // write out at sample freq
    cur_sample ++;
    if (cur_sample >= sample) {
      cur_sample = 0;
      write_dbls(2, cur_param, y);
      //print_dbls(2, cur_param, y);
    }
    
    // update vals to next val
    y += dY * step;
  
  }

  end_sim();
}

