#include <stdio.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>


class Entry {
public:
  char *token;
  int freq;
  
  Entry(char *_token, int _freq): token(_token), freq(_freq) {};
  ~Entry(){
	free(token);
  }
};
  
char *read_string(char *beg, char *end){
  int len = end-beg+1;
  char *newstring = (char*) malloc(len * sizeof(char));
  if (newstring == NULL){
	printf("Malloc error.\n");
	exit(1);
  }
  for(int i = 0; i < len-1; i++){
	newstring[i] = beg[i];
  }
  newstring[len-1] = 0;
  return newstring;
}

Entry *read_entry(char *buf, char **next){
  char *token, *sfreq, *ptr, *nptr;
  int freq;
  for(ptr = buf; *ptr != '\t'; ptr++){
	if(*ptr == '\n'){
	  char *problem = read_string(buf, ptr);
	  printf("Parsing error: %s", problem);
	  free(problem);
	}
  }
  token = read_string(buf, ptr);
  for(nptr = ptr+1; *nptr != '\n'; nptr++);
  sfreq = read_string(ptr+1, nptr);
  freq = atoi(sfreq);
  free(sfreq);
  *next = nptr+1;
  return new Entry(token, freq);
}


Entry **read_freqdist(char *buf, size_t nlines){
  char *ptr = buf, *next = buf;
  Entry **fd = (Entry**) malloc(nlines * sizeof(Entry*));
  if(fd == NULL){
	printf("Error allocating freqdist.\n");
	exit(1);
  }
  for(size_t i = 0; i < nlines; i++){
	fd[i] = read_entry(ptr, &next);
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

void free_fd(Entry **fd, size_t nlines){
  for(size_t i = 0; i < nlines; i++){
	delete fd[i];
  }
  free(fd);
}

Entry **omorfize(Entry **fd, size_t numlines){
  for(size_t i = 0; i < numlines; i++){
     
  }
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
  Entry **fqd = read_freqdist(buf, nlines);
  printf("Freqdist read.");
  omorfize(fqd, nlines);
  free_fd(fqd, nlines);

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

  for (i = 1; i < argc; i++){
	stemfile(argv[i]);
  }
  
  return 0;
}
