
//#include "svdpi.h"

extern void writew(unsigned int addr, unsigned int data);
extern void readw(unsigned int addr, unsigned int *data);
extern void wait_cycles(int n);
extern void report_info(const char* msg);
extern void report_info_para(const char* msg, int para);
extern void report_warning(const char* msg);
extern void report_error(const char* msg);
extern void report_fatal(const char* msg);
extern void finish();
extern void wait_time(int nb, const char* unit);


#define MCDF_REG_ADDR_START 0x0000 
#define MCDF_REG_ADDR_END   0x0FFF 
#define MCDF_CH0_ADDR_START 0x1000 
#define MCDF_CH0_ADDR_END   0x10FF 
#define MCDF_CH1_ADDR_START 0x1100 
#define MCDF_CH1_ADDR_END   0x11FF 
#define MCDF_CH2_ADDR_START 0x1200 
#define MCDF_CH2_ADDR_END   0x12FF 
#define MCDF_CH3_ADDR_START 0x1300 
#define MCDF_CH3_ADDR_END   0x13FF 
#define MCDF_FMT_ADDR_START 0x2000 
#define MCDF_FMT_ADDR_END   0x2FFF 

int diff_value(unsigned int val1, unsigned int val2) {
  if(val1 != val2) {
    report_info_para("val1 = %8x", val1);
    report_info_para("val2 = %8x", val1);
    report_error("EROR! val1 != val2");
    return 0;
  }
  else {
    report_info_para("SUCCESS! val1 == val2, value = %8x", val1);
    return 1;
  }
}


int dpi_c_thread(void) {
  unsigned int wr_val, rd_val;
  unsigned int i, j;
  unsigned int is_equal;
  wr_val = (0x1<<3) + (0x1<<2) + (0x1<<1) + 1;
  writew(MCDF_REG_ADDR_START+0x00, wr_val);
  readw(MCDF_REG_ADDR_START+0x00, &rd_val);
  report_info("COMPARE SLV_EN_REG write value and read value");
  is_equal = diff_value(wr_val, rd_val);

  wr_val = (63<<24) + (31<<16) + (15<<8) + 7;
  writew(MCDF_REG_ADDR_START+0x0C, wr_val);
  readw(MCDF_REG_ADDR_START+0x0C, &rd_val);
  report_info("COMPARE SLV_LEN_REG write value and read value");
  is_equal = diff_value(wr_val, rd_val);

  for(i=0; i<16; i++) {
    for(j=0; j<8; j++) {
      writew(MCDF_CH0_ADDR_START, (i<<16)+j);
    }
  }
  for(i=0; i<16; i++) {
    for(j=0; j<16; j++) {
      writew(MCDF_CH1_ADDR_START, (i<<16)+j);
    }
  }
  for(i=0; i<16; i++) {
    for(j=0; j<32; j++) {
      writew(MCDF_CH2_ADDR_START, (i<<16)+j);
    }
  }
  for(i=0; i<16; i++) {
    for(j=0; j<64; j++) {
      writew(MCDF_CH3_ADDR_START, (i<<16)+j);
    }
  }
  wait_time(1, "us");
  return 0;
}
