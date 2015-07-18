#ifndef __STEMCORP_H__
#define __STEMCORP_H__
#include <stdio.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string>
#include <map>
#include <vector>


typedef unsigned int freq;
typedef std::map<std::string, freq> fdmap;
class Entry;

class FreqDist {
private:
  fdmap innermap;
  
public:
  FreqDist() {};
  FreqDist(const FreqDist &other) : innermap(other.innermap) {};
  
  void add (const Entry&);
  void add (std::string, freq);
  inline size_t size() { return innermap.size(); }
  FreqDist *clean_fd();
  void write(const std::string);
};

class Entry {
public:
  std::string token;
  freq fq;
  
  Entry(std::string _token, freq _freq): token(_token), fq(_freq) {};
};
  
typedef enum {ANALYZE, GENERATE} omorfi_task;
class Omorfi {
private:
  int p_in, p_out;
  pid_t child;
public:
  Omorfi(omorfi_task);

  ~Omorfi();

  std::vector<std::string> get_analyses(const std::string);

};


#endif
