// Trickier storage live test with variable shadowing and nested scopes

fn main() {
    let x = 10;

    {
        let x = 20;  
        let t = x + 5;  
    } 
    
    
    let r = x + 100;
    
    let mut x = 30;
    x = x + 10;
}