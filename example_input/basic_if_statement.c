void main(){
	int a = 40;
    int b = 10;
    if (a == 40){
        print("a is equal to 40");
        if(b==10){
            print(1);
        } else if(b>10){
            print(2);
        } else if(b<10){
            print(3);
        } else {
            print(4);
        }
    } else if (a > 40){
        print("a is larger than 40");
        if(b==10){
            print(11);
        } else if(b>10){
            print(12);
        } else if(b<10){
            print(13);
        } else {
            print(14);
        }
    } else{
        print(666);
        if(b==10){
            print(21);
        } else if(b>10){
            print(22);
        } else if(b<10){
            print(23);
        } else {
            print(24);
        }
    }

	return;
}

