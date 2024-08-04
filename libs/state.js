import CryptoES from 'crypto-es';


export const simpleHash = str => {
  return CryptoES.MD5(str).toString()
};

export class KernelState {
  state = {}
  hash = ''
  
  constructor (state = undefined, ev) {
    if (state) this.state = {...state.state};
    const code = simpleHash(String(ev.uid) + String(ev.pattern));
    this.state[code] = ev.data;
    const self = this;
    this.hash = Object.keys(this.state).reduce((acc, e) => {
      const h = simpleHash('dt'+String(self.state[e])) + simpleHash('st'+String(e));
      return acc + h;
    }, 0);
    return this;
  }
  
  set (o, m) {
    const h = String(this.hash);
    if (!m.has(h)) m.set(h, {});
    const object = m.get(h);
    if (o.name in object) {
      object[o.name].set.push(o.data);
    } else {
      object[o.name] = {i:0, set:[o.data]} 
    }
  }
  
  exec (m, fn) {
    const h = String(this.hash);
    if (!m.has(h)) {
      console.log('State does not exists!');
      console.log(this.state);
      return false;
    }
    
    fn(m.get(h));
  }
}