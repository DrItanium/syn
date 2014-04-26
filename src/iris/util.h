
typedef struct FileWrapper {
   FILE* fptr;
   char* line;
   char* permissions;
   int needsclosing;
} FileWrapper;

void openfw(FileWrapper* fw);
void closefw(FileWrapper* fw);
