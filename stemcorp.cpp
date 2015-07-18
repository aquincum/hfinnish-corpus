#include "stemcorp.h"


void FreqDist::add(const Entry &e){
  if(innermap.count(e.token) == 0){
	innermap[e.token] = e.fq;
  }
  else {
	innermap[e.token] += e.fq;
  }
}

void FreqDist::add(std::string s, freq fq){
  this->add(Entry(s,fq));
}

  
std::string read_string(const char *beg, const char *end){
  int len = end-beg+1;
  //char *newstring = (char*) malloc(len * sizeof(char));
  std::string newstring(len, 0);
  /*  if (newstring == NULL){
	printf("Malloc error.\n");
	exit(1);
	}*/
  for(int i = 0; i < len-1; i++){
	newstring[i] = beg[i];
  }
  return newstring;
}

Entry *read_entry(const char *buf, char **next){
  std::string  token, sfreq;
  char *ptr, *nptr;
  freq fq;
  for(ptr = (char*) buf; *ptr != '\t'; ptr++){
	if(*ptr == '\n'){
	  std::string problem = read_string(buf, ptr);
	  printf("Parsing error: %s", problem.c_str());
	}
  }
  token = read_string(buf, ptr);
  for(nptr = ptr+1; *nptr != '\n'; nptr++);
  sfreq = read_string(ptr+1, nptr);
  fq = (freq) atoi(sfreq.c_str());
  *next = nptr+1;
  return new Entry(token, fq);
}


FreqDist *read_freqdist(char *buf, size_t nlines){
  char *ptr = buf, *next = buf;
  Entry *current;
  FreqDist *fd = new FreqDist();
  if(fd == NULL){
	printf("Error allocating freqdist.\n");
	exit(1);
  }
  for(size_t i = 0; i < nlines; i++){
	current = read_entry(ptr, &next);
	fd->add(*current);
	delete current;
	ptr = next;
  }
  return fd;
}

 
size_t count_lines(char *buf, size_t filesize){
  int cnt;
  for(size_t i = 0; i < filesize; i++){
	if (buf[i] == '\n') cnt += 1;
  }
  if( buf[filesize-1] != '\n' ) cnt += 1;
  return cnt;
}


FreqDist *omorfize(FreqDist *fd, size_t numlines){
  for(size_t i = 0; i < numlines; i++){
     
  }
}

const std::string legal_chars = "abcdefghijklmnopqrstuvwxyzäö";
std::string clean_string(std::string s){
  std::string rv;
  for(std::string::iterator i = s.begin(); i != s.end(); i++){
	if(legal_chars.find(*i) != std::string::npos) {
	  rv.push_back(*i);
	}
  }
  return rv;
}

FreqDist *FreqDist::clean_fd(){
  FreqDist *rv = new FreqDist();
  for(auto it = this->innermap.begin(); it != this->innermap.end(); it++){
	rv->add( clean_string(it->first), it->second);
  }
  return rv;
}

void FreqDist::write(const std::string fname){
  FILE *f = fopen(fname.c_str(), "wt");
  if(f == NULL){
	printf("Error: cannot open %s for writing.\n", fname.c_str());
	exit(1);
  }
  
  for(auto it = this->innermap.begin(); it != this->innermap.end(); it++){
	const char *s = it->first.c_str();
	freq fq = it->second;
	fprintf(f, "%s\t%d\n", s, fq);
  }
  fclose(f);
}


Omorfi::Omorfi(omorfi_task task){
  int inpipes[2], outpipes[2];
  if( pipe(inpipes) == -1 || pipe(outpipes)){
	printf("Error opening pipes\n");
	exit(1);
  }
  fprintf(stderr, "inpipe0=%d, inpipe1=%d, outpipe0=%d, outpipe1=%d\n", inpipes[0], inpipes[1], outpipes[0], outpipes[1]);

  child = fork();
  if(child == 0){  // I am the child
	std::string prog;
	switch (task){
	case ANALYZE:
	  prog = "omorfi-interactive.sh";
	  break;
	case GENERATE:
	  prog = "omorfi-generate.sh";
	  break;
	}
	if(dup2(inpipes[1],STDOUT_FILENO) == -1){
	  fprintf(stderr,"DUP2 problem 1\n");
	  exit(1);
	}
	if(dup2(outpipes[0],STDIN_FILENO) == -1){
	  fprintf(stderr,"DUP2 problem 2\n");
	  exit(1);
	}
	close(inpipes[0]);
	close(inpipes[1]);
	close(outpipes[0]);
	close(outpipes[1]);
	/*
	if(execlp(prog.c_str(), prog.c_str(), (char*) NULL) == -1){
	  fprintf(stderr,"EXEC problem\n");
	  exit(1);
	  
	  }*/
	while (1){
	  fprintf(stdout, "alma\n");
	  sleep(1);
	}
	fprintf(stderr,"OMORFI DONE\n");
	exit(0);
  }
  else {
	this->child = child;
	close(outpipes[0]);
	close(inpipes[1]);
	p_in = inpipes[0];
	p_out = outpipes[1];
  }
}

Omorfi::~Omorfi(){
  close(p_in);
  close(p_out);
  kill(child, SIGTERM);
}

#define BUFLEN 2014
std::vector<std::string> Omorfi::get_analyses(const std::string wd){
  char buf[BUFLEN];
  int i = 0, bytesav, tries = 0;
  fprintf(stderr, "pout=%d, pin=%d\n", p_out, p_in);
  write(p_out, wd.c_str(), wd.length());
  write(p_out, "\nx\nx\nx\nx\njoulun\n", 1);
  while(1){
	fprintf(stderr,"IO: %d\n",ioctl(p_in, FIONREAD, &bytesav));
	if(bytesav) {
	  break;
	}
	tries ++;
	puts(wd.c_str());
	puts(".");
	sleep(1);
	if(tries > 5){
	  printf("Omorfi timeout\n");
	  return std::vector<std::string>();
	}
  }
  read(p_in, (void*) buf, BUFLEN);
  printf("! %s\n", buf);
}

void stemfile(char *fname){
  size_t fsize;
  char *buf;
  int fd;
  struct stat file_stat;

  if(stat(fname, &file_stat) == -1){
	printf ("Error stating %s.\n", fname);
	exit(1);
  }

  fsize = (size_t) file_stat.st_size;
  fd = open(fname, O_RDONLY);
  if(fd == -1){
	printf ("Error opening %s.\n", fname);
	close(fd);
	exit(1);
  }
  buf = (char*) mmap(NULL, fsize, PROT_READ, MAP_PRIVATE, fd, (off_t) 0);

  if (buf == MAP_FAILED){
	printf("Error mmap %s.\n", fname);
	close(fd);
	exit(1);
  }
  size_t nlines = count_lines(buf, fsize);
  FreqDist *fqd = read_freqdist(buf, nlines);
  printf("Freqdist read. n=%d\n", (int) fqd->size());
  FreqDist *cleaned = fqd->clean_fd();
  printf("Freqdist cleaned. n=%d\n", (int) cleaned->size());
  cleaned->write("cleaned");
  // omorfize(fqd, nlines);
  
  delete fqd, cleaned;
  if(munmap(buf, fsize) == -1){
	printf("Error munmap %s.\n", fname);
	close(fd);
	exit(1);
  }

  close(fd);
}


int main(int argc, char **argv){
  int i;
  
  if(argc < 2){
	printf("Usage: stemcorp <freqdists>\n");
	return 1;
  }
  /*
  for (i = 1; i < argc; i++){
	stemfile(argv[i]);
	}*/

  //TEST
  Omorfi om(ANALYZE);
  om.get_analyses("olen");
  
  return 0;
}
