r = "\e[43;3#{C="#{n*5%9+1}m"}#{T}\e[4"+C+S[1568,79]+E="\e[0m"; r[81,21]="\e[37m#{(["Caf\u00e9_au_lait","Yogurt","Fruit_mix"][n].chars*Z).tr(?_,"").center(21)}\e[3"+C;
a = %~POSA[`ER]`PASX1cTc22V6NNP.QOYGMXXIG7KK:bCCaVN8WZ[]UQMMS`cBFFJJHHY`QTUIUURRPTOcRV_a LLUT`WXLW]a_c_bV`XXYa_9}+[T='  B  A  L  A  N  C  E  D   F  O  O  D  ']*0+%w{bZZYb_][9cc ????`9^acGG,,N9DU`DKcUKU3K4!4!4!QXTSSS""9`9`#U`KcK--S;;/GOT<QE$U=>F==Q0@%U/P/B=S0Q`PM&XVV V15CMRHMSHRKO>==QMQVR'b`&DK>BS<XE$T>T33DDDUM<V@@E(((TCT0A<0A"')5CXPcQa54X@@Y#KcK--S;;/GOT<Q`$)T)T:a4A%%#XVS6a'b`&DK>BS<T7**]^^b6+++]~;
P=Struct.new(:x,:d,:p,:v0);
M=(-5**7..b=0).map{[]};
A=s=[];
t=Time.now;
q=?y.succ;
(
  s=S.scan(/.+/ );
  M[0]<<P[25i-b%3*5i-9,0,0,2+1i];
  60.times{|i|
    j = i % 20;
    i < 40 ? [M[j-1],m=M[j],M[j+1]].each{|n|
      m.each{|p|
        n.each{|q|
          d=p.x-q.x;
          w=d.abs-4;
          w<0 && (
            i<20 ? p.d+=w*w : p.p+=w*(
              d*(3-p.d-q.d) + (p.v-q.v)*4
            )/p.d
          )
        }
      }
    }
    :M.shift.each{|p|
      y,x = (p.x+=p.v+=p.p/10).rect;
      p.p = [43-b/9.0-y,1].min - [x,p.d=0,x-92].sort[1]*2i;
      p.v /= [1,p.v.abs/2].max;
      M[20-j+[0,(x+4).div(5),19].sort[1]]<<p;
      35.times{|w|
        v=x.to_i-3+w%7;
        c=s[w=y.div(2)-2+w/7];
        (x-v)**2+(y-w*2)**2 < 16 && 0<=w && c && (k=(w*2-21)**2/99)<=v && c[v] && k+79!=v && c[v]=q
      }
    }
  };
  (24-(b)/18..21).map{|k|
    s[k]=Z*(k=(k*2-21)** 2/99)+q*79+Z+q*2*(6-k)
  };
  s*="\e[B\r";
  "  Your favorite flavor  ";
  b+=1;
  A<<"\e[A\r"*21+s.gsub(/\172+/){
    "\e[43m"+$&.tr(q,Z)+E
  }
) while (+s.count(q))<1950;
A.map{|q|
  sleep([t-Time.now+3,2e-2].max);
  $><<s=q
};
$><<s.gsub(?m,";33m").gsub(Z){
  S.slice!(/./)
};
b=?]*33.upto(91){|i|
  a=~/../ ;
  a=$'.gsub(i.chr,$&)
}*2;
Z<<8;
(b+a.gsub(?^,"^]"*41)+b).bytes{|c|
  c-=86;
  c<8 ? sleep(3e-2) : $><<(c<('CalorieMate-Liquid-Quine'; 9) ? r.slice!(/\e.*?m|./) : c>9 ? "\e[%X"%c : Z)
}
