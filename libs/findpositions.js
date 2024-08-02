export function filterKeys(object, filterFunction) {
    // Create a new object to store the filtered key-value pairs
    const filteredObject = {};
  
    // Iterate over each key in the input object
    for (const key in object) {
      // Check if the property is a direct property of the object
      if (object.hasOwnProperty(key)) {
        // Apply the filter function to the key
        if (filterFunction(key)) {
          // If the filter function returns true, add the key-value pair to the filtered object
          filteredObject[key] = object[key];
        }
      }
    }
  
    // Return the filtered object
    return filteredObject;
  }
  
  function analyzeTimeSeries(data) {
      const letterNumberMap = {};
      const letterCount = {};
      const totalConnections = {};
  
      let i = 0;
      while (i < data.length) {
          const char = data[i];
          if (isNaN(char)) {
              // It's a letter
              if (!letterCount[char]) {
                  letterCount[char] = 0;
              }
              letterCount[char]++;
              
              // Check following characters for numbers
              i++;
              while (i < data.length && !isNaN(data[i])) {
                  const num = data[i];
                  if (!letterNumberMap[char]) {
                      letterNumberMap[char] = {};
                  }
                  if (!letterNumberMap[char][num]) {
                      letterNumberMap[char][num] = 0;
                  }
                  letterNumberMap[char][num]++;
                  if (!totalConnections[char]) {
                      totalConnections[char] = 0;
                  }
                  totalConnections[char]++;
                  i++;
              }
          } else {
              i++;
          }
      }
  
      // Calculate probabilities
      const probabilities = {};
      for (let letter in letterNumberMap) {
          probabilities[letter] = {};
          for (let number in letterNumberMap[letter]) {
              probabilities[letter][number] = letterNumberMap[letter][number] / totalConnections[letter];
          }
        const max = Math.max(...Object.values(probabilities[letter]));
        Object.keys(probabilities[letter]).map((n) => probabilities[letter][n]=probabilities[letter][n]/max );
      }
  
      return { letterNumberMap, probabilities, letterCount };
  }
  function findCorrespondences(timeSeries, connections) {
      const correspondences = [];
      const pending = [];
      let i = 0;
  
      while (i < timeSeries.length) {
          const char = timeSeries[i];
          if (isNaN(char) && !connections[char]) {
            i++;
            continue;
          }
        
          if (isNaN(char)) {
            pending.push({type: char, elements: [], pos: i});
          } else {
            pending[0].elements.push({data: char, pos: i});
            if (pending[0].elements.length == connections[pending[0].type].length) {
              correspondences.push(pending.shift());
            }
          }
          i++;
      }
  
      return correspondences;
  }
  
  export function findPositions(timeSeries) {
  // Example usage
  const result = analyzeTimeSeries(timeSeries);
  
  console.log("Connections:", result.letterNumberMap);
  console.log("Probabilities:", result.probabilities);
  console.log("Letter Counts:", result.letterCount);
  
  let connected = {};
  Object.keys(result.letterNumberMap).forEach((letter) => {
    connected[letter] = [];
    Object.keys(result.probabilities[letter]).forEach((num) => {
      if (result.probabilities[letter][num] >= 0.5) connected[letter].push(String(num));
    });
  });
    
    const structure = findCorrespondences(timeSeries, connected);
    
    console.warn(connected);
    const connectionGroups = splitIntoNonOverlappingGroups(connected);
    console.warn(connectionGroups);
    console.warn('fuck');
    
    const groups = [];
    connectionGroups.forEach((group) => {
      groups.push({structure: structure.filter((e) => (e.type in group)), count:filterKeys(result.letterCount, (e) => (e in group)), probabilities: filterKeys(result.probabilities, (e) => (e in group)),  connections: group});
    });
  
    
    return groups;
  
  
  //return {structure: findCorrespondences(timeSeries, connected), connections: connected}
  }
  function splitIntoNonOverlappingGroups(obj) {
      // Function to check if two arrays intersect
      function arraysIntersect(arr1, arr2) {
          return arr1.some(item => arr2.includes(item));
      }
  
      // Initialize an array to hold the result groups
      let result = [];
  
      // Iterate over each key-value pair in the object
      for (let key in obj) {
          let newGroup = {};
          newGroup[key] = obj[key];
  
          // Track which groups intersect with the current key-value pair
          let intersectingGroups = [];
  
          for (let group of result) {
              let groupKeys = Object.keys(group);
              let groupValues = groupKeys.reduce((acc, k) => acc.concat(group[k]), []);
  
              // Check if there's an intersection with the current group
              if (arraysIntersect(groupValues, obj[key])) {
                  intersectingGroups.push(group);
              }
          }
  
          // Merge intersecting groups and add the new key-value pair
          if (intersectingGroups.length > 0) {
              // Create a combined group
              let combinedGroup = intersectingGroups.reduce((acc, group) => {
                  return Object.assign(acc, group);
              }, newGroup);
  
              // Remove the old intersecting groups
              result = result.filter(group => !intersectingGroups.includes(group));
  
              // Add the combined group to the result
              result.push(combinedGroup);
          } else {
              // If no intersecting group found, add the new group to the result
              result.push(newGroup);
          }
      }
  
      return result;
  }
  
  