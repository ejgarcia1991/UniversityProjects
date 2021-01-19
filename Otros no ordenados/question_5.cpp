#include<iostream>

using namespace std;
/*
bool isVowel(char ch){
    if(()
        return true;
    else
        return false;
}*/

int main(){
    int counter=0;
    char ch;
    cout<<"enter a sequence of character:";
    cin>>ch;
    while(ch!='#'){

        if((ch=='a')||(ch=='e')||(ch='i')||(ch=='o')||(ch=='u'))
        {
            cout<<ch<<endl;
            counter++;
        }
        cin>>ch;
    }
    cout<<"number of vowel "<<counter;

    return 0;
}
