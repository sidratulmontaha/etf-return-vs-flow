f = new XMLHttpRequest()

f.open("POST", "http://www.etf.com/etf-finder-funds-api//-aum/0/3000/1")
f.onreadystatechange = function(){
  if(f.readystate == 4 && f.status == 200){
    console.log(f.responseText);
  }
}
f.send()