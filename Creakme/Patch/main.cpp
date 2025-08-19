#include <stdio.h>
#include <assert.h>

int main(int argc, char** argv)
{
    if (argc != 2)
    {
        printf ("Command line contain only file name\n");
        return 0;
    }

    FILE* file = fopen (argv[1], "r+");
    if (!file)
    {
        printf ("Error opening or this file does not exist\n");
        return 0;
    }

    int c = 0;
    int p = 0;
    int n = 0;

    while ((c = fgetc(file)) != EOF)
    {
        if (c == 0x74)
        {
            if (p < 0x61 || p > 0x7A)
            {
                if ((n = fgetc (file) < 0x61 || n > 0x7A))
                {
                    fseek (file, -2, SEEK_CUR);
                    fputc (0xEB, file);
                }
                else
                    fseek (file, -1, SEEK_CUR);
            }
            
            p = c;
        }
    }

    fclose (file);
}