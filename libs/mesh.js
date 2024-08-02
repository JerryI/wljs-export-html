export const eventNameToString = (ev) => (String(ev.uid) + String(ev.pattern))

export class KernelMesh {
    constructor(group, database) {
      this.database = database;
      this.whitelist = Object.keys(group.eventObjects);
      return this;
    }
    
    test(msg) {
  
      return this.whitelist.includes(eventNameToString(msg));
    }
    
    serialize() {
      return encodeURIComponent(JSON.stringify({db:Object.fromEntries(this.database), wl:this.whitelist}));
    }
    
    static unpack(string) {
      const data = JSON.parse( decodeURIComponent( string ));
      const o = new KernelMesh({database: new Map(Object.entries(data.db)), whitelist: data.wl});
      return o;
    }
}