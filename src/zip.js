import { unzlibSync } from 'fflate'; 

interpretate.unzlib64String = (input) => {
    const decoded = atob(input);
    return new TextDecoder().decode(unzlibSync(Uint8Array.from(decoded, c => c.charCodeAt(0))));
}
