#include<stdio.h>
#include<stdlib.h>
#include<time.h>


int main()
{
    srand(time(NULL));
    printf("P1\n");
    printf("8 8\n");
    for(int i =1 ; i<=8;i++)
    {
        for(int j = 1;j<= 8;j++){
            if(j<=4 && i <= 4)
            {
                printf("0 ");
            }
            else{
                printf("%d",(rand() % 2));
                if(j != 8)
                    printf(" ");
            }
            
        }
        printf("\n");
    }
    printf("4\n");
    return 0;
}