core["Notebook`Editor`ExportNotebook`Internal`Sniffer"] = async (args, env) => {
    const cmd = await interpretate(args[0], env);
    core["Notebook`Editor`ExportNotebook`Internal`Sniffer"][cmd](args.slice(1), env);
};

core["Notebook`Editor`ExportNotebook`Internal`Sampler"] = async (args, env) => {
    const cmd = await interpretate(args[0], env);
    return core["Notebook`Editor`ExportNotebook`Internal`Sampler"][cmd](args.slice(1), env);
};

let instance = {};

let SamplerNode = false;

core["Notebook`Editor`ExportNotebook`Internal`Sampler"].Init = async (args, env) => {
    const channel = await interpretate(args[0], env);
    if (!SamplerNode) SamplerNode = (await import('./sampler-e2164a6f.js')).SamplerNode;
    instance.sampler = new SamplerNode(instance, channel);

    instance.sampler.emitter = (ev) => {
        server.kernel.emitt(ev.uid, ev.data, ev.pattern);
    };

    core.WLJSIOUpdateSymbol = (args, env) => {
        const name = interpretate(args[0], env);
        instance.sampler.recieve({type: 'symbol', name: name, data: args[1]});
        instance.old.symbol(args, env);
    };

    instance.sampler.start();
};

core["Notebook`Editor`ExportNotebook`Internal`Sampler"].Stop = async (args, env) => {
    instance.sampler.stop();
    instance.restore();
};

core["Notebook`Editor`ExportNotebook`Internal`Sampler"].Dispose = async (args, env) => {
    instance.sampler.dispose();
};

core["Notebook`Editor`ExportNotebook`Internal`Sampler"].Get = async (args, env) => {
    const c = instance.sampler.pump();
    console.log(c);
    return c;
};

core["Notebook`Editor`ExportNotebook`Internal`Sniffer"].Eject = async (args, env) => {
    instance.restore();
};

core["Notebook`Editor`ExportNotebook`Internal`Sniffer"].Purge = async (args, env) => {
    delete instance.dump;
};

let AnalyzerNode = false;

core["Notebook`Editor`ExportNotebook`Internal`Sniffer"].Get = async (args, env) => {
    const node = new AnalyzerNode(instance.dump);
    node.analyze();
    const groups = node.makeGroups();

    return [encodeURIComponent(JSON.stringify(instance.dump)), groups];
};

core["Notebook`Editor`ExportNotebook`Internal`Sniffer"].GetRaw = async (args, env) => {
    const node = new AnalyzerNode(instance.dump);
    node.analyze();
    const groups = node.makeGroups();

    return encodeURIComponent(JSON.stringify({dump: instance.dump, groups: groups}));
};

core["Notebook`Editor`ExportNotebook`Internal`Sniffer"].Inject = async (args, env) => {
    const channel = await interpretate(args[0], env);

    const reciver = server.kernel.emitt.bind(server.kernel);
    const updateSym = core.WLJSIOUpdateSymbol;

    instance = {
        old: {event: reciver, symbol:updateSym},
        restore: () => {
            server.kernel.emitt = instance.old.event;
            core.WLJSIOUpdateSymbol = instance.old.symbol;
            
            console.log('Restored!');
        },
        dump: [],
        handlers: [],
        channel: channel
    };

    if (!AnalyzerNode) AnalyzerNode = (await import('./analyzer-4aa51cef.js').then(function (n) { return n.a; })).AnalyzerNode;

    let time = performance.now();
    let block = false;
    instance.handlers.push((object) => {
        if (block) return;
        if (performance.now() - time < 500) return;
        time = performance.now();
        if (instance.dump.length < 10) return;

        if (instance.dump.length > 15*60*2) {
            block = true;
            instance.restore();
            server.emitt(channel, '"Too many events. Recording was suspended"', 'Message');
        }

        const node = new AnalyzerNode(instance.dump);
        node.analyze();
        console.log(node);
        const groups = node.makeGroups();
        server.emitt(channel, '"'+encodeURIComponent(JSON.stringify(groups))+'"', 'Stat');
    });

    server.kernel.emitt = function(uid, data, type = 'Default') {
      instance.dump.push({type: 'event', uid: uid, pattern: type, data:data});
      //instance.handlers.forEach((h) => h(instance));

      reciver(uid, data, type);
    };
     
    core.WLJSIOUpdateSymbol = (args, env) => {
      const name = interpretate(args[0], env);

      instance.dump.push({type: 'symbol', name: name, data: args[1]});
      instance.handlers.forEach((h) => h(instance));

      updateSym(args, env);
    };    
};
