import CryptoES from 'crypto-es';


export const simpleHash = str => {
  return CryptoES.MD5(str).toString()
};

const permutator = (inputArr) => {
  let result = [];

  const permute = (arr, m = []) => {
    if (arr.length === 0) {
      result.push(m)
    } else {
      for (let i = 0; i < arr.length; i++) {
        let curr = arr.slice();
        let next = curr.splice(i, 1);
        permute(curr.slice(), m.concat(next))
     }
   }
 }

 permute(inputArr)

 return result;
}

export class KernelState {
  state = {}
  hash = ''
  
  constructor (state = undefined, ev, ffast = false) {
    if (state) this.state = {...state.state};
    const code = String(ev.uid) + String(ev.pattern);
    this.state[code] = ev.data;
    const self = this;
    if (ffast) {
      this.hash = [Object.keys(this.state)].map((variant) => variant.reduce((acc, e) => {
        const h = 'dt'+String(self.state[e]) + 'st'+String(e);
        return acc + h;
      }, 0));

      return this;
    }

    this.hash = permutator(Object.keys(this.state)).map((variant) => variant.reduce((acc, e) => {
      const h = 'dt'+String(self.state[e]) + 'st'+String(e);
      return acc + h;
    }, 0));


    return this;
  }
  
  set (o, m) {
    let found = false;

    for (const h of this.hash) {
      if (m.has(h)) {
        found = h;
        break;
      }
      
    }

    if (!found) {
      found = this.hash[0];
      m.set(found, {});
      for (let k = 1; k<this.hash.length; ++k) {
        m.set(this.hash[k], {fwd: found});
      }
    }
    
    const object = m.get(found);
    //object.state = {...this.state};//debug only
    if (o.name in object) {
      object[o.name].set.push(o.data);
    } else {
      object[o.name] = {i:0, set:[o.data]} 
    }
  }
  
  exec (m, fn) {
    let h = this.hash[0];
    while(m.has(h)) {
      const o = m.get(h);
      if (o.fwd) {
        h = o.fwd;
        continue;
      }

      return fn(o)
    }

    console.error('State does not exists!');
    console.log(this.state);
    console.log(this.hash);
  }
}