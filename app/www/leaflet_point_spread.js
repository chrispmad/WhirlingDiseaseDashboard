$(document).on('shiny:connected', function(event) {
  // Wait for the document to be loaded.
  
  // Next, wait for the leaflet map to be populated with all of the datapoints.
  let number_of_markers_on_map = 0;
  let map_finished_rendering = false;

  // Functions
  function createClone(original) {
    const clone = original.cloneNode(true);
    clone.style.position = 'fixed';
    clone.style.pointerEvents = 'none';
    //clone.style.transform = 'translate(-50%, -50%) scale(1.5)';
    clone.style.transition = 'transform 1s ease';
    clone.classList.add('hover-clone');
    return clone;
  }
  
  function addToTranslate3d(transformStr, dx = 0, dy = 0, dz = 0) {
    const match = transformStr.match("translate3d") == "translate3d"
    
    if (!match) return `translate3d(${dx}px, ${dy}px, ${dz}px)`; // fallback: not in translate3d format
  
    const x = parseFloat(match[1]) + dx;
    const y = parseFloat(match[2]) + dy;
    const z = parseFloat(match[3]) + dz;
  
    console.log(`translate3d(${x}px, ${y}px, ${z}px)`);
    
    return `translate3d(${x}px, ${y}px, ${z}px)`;
  }
  
  const intervalId = setInterval(() => {
    // Count all kinds of interactive leaflet markers.
    const markers = document.querySelectorAll('.leaflet-interactive');
    // Subset the above nodeList for just those of type path (parasite markers)
    const parasite_markers = Array.from(markers).filter(el => 
      el.tagName === 'path'
    );
    // ... for type image that indicate Tubifex results
    const tubifex_markers = Array.from(markers).filter(el => 
      el.tagName === 'IMG' && el.src.includes("https://www.freeiconspng.com/uploads/orange-triangle-image-vector-0.png")
    );
    // ... for type image that indicate Fish sampling
    const fish_markers = Array.from(markers).filter(el => 
      el.tagName === 'IMG' && el.src.includes("https://www.freeiconspng.com/uploads/orange-square-image-2.png")
    );
    // console.log("There are this many fish markers: " + fish_markers.length)
    // Compare if the number of markers currently detected is different from
    // the last time this interval ran. If it is not different, leaflet has 
    // likely finished making the map.
    
    if(markers.length == number_of_markers_on_map && number_of_markers_on_map != 0){
      map_finished_rendering = true;
    } else {
      // Number of markers detected was different from the last interval. Update
      // the variable that tracks the number of markers on the map.
      number_of_markers_on_map = markers.length;
    }
    
    // Update our tracker of the number of markers.
    if (map_finished_rendering) {
      
      console.log('There are currently ' + markers.length + ' markers on the map.');
      
      // Stop running the interval.
      clearInterval(intervalId);
      
      // Set up each marker to get cloned when it is moused over, then
      // made invisible, and its clone is moved a bit.
      markers.forEach(marker => {
        // Test to make sure the marker the user mouses over isn't one of the subwatersheds.
        // To identify these, we test what each 'PATH' type div has as its stroke colour.
        // If it's grey, we treat it as something to NOT have the cloning code applied to.
        if(window.getComputedStyle(marker).stroke != 'rgb(128, 128, 128)'){
          marker.addEventListener('mouseenter', function (e) {
            const rect = marker.getBoundingClientRect();
            const clone = createClone(marker);
            //clone.style.left = rect.left + rect.width / 2 + 'px';
            //clone.style.top = rect.top + rect.height / 2 + 'px';
            clone.style.transition = '1s all ease';
            clone.style.pointerEvents = 'none';
            marker.style.opacity = 0;
            clone.style.opacity = 1;
            
            // Find the initial transform.
            let initial_transform = marker.style.transform;
            
            // Add clone to the hover clone pane.
            document.getElementsByClassName('hover-clone-pane')[0].appendChild(clone);
            
            // Conditionally apply movement transforms to the clone based on what
            // type they are.
            if(marker.tagName === 'path' && window.getComputedStyle(marker).stroke != 'rgb(128, 128, 128)'){
              const updated = addToTranslate3d(initial_transform, 10, 0, 0); // Add 15px to X
              console.log("transform before shift: " + clone.style.transform);
              clone.style.transform = updated;
              console.log("transform after shift: " + clone.style.transform);
              console.log("Moved a marker of type 'path'")
            }
            if(marker.tagName === 'IMG' && marker.src.includes("orange-triangle-image-vector-0")){
              const updated = addToTranslate3d(initial_transform, -10, 0, 0); // Add 15px to X
              console.log("transform before shift: " + clone.style.transform);
              clone.style.transform = updated;
              console.log("transform after shift: " + clone.style.transform);
              console.log("Moved a marker of type 'image' and is a triangle")
            }
            // ... for type image that indicate Fish sampling
            if(marker.tagName === 'IMG' && marker.src.includes("orange-square-image-2")){
              const updated = addToTranslate3d(initial_transform, -5, -5, 0); // Add 15px to X
              console.log("transform before shift: " + clone.style.transform);
              clone.style.transform = updated;
              console.log("transform after shift: " + clone.style.transform);
              console.log("Moved a marker of type 'image' and is a square")
            }
            
          });
    
          marker.addEventListener('mouseleave', function (e) {
            const container = document.getElementsByClassName('hover-clone-pane')[0];
            container.innerHTML = '';
            //marker.style.visibility = 'visible';
            marker.style.opacity = 1;
          });
        }
      });
    }
  }, 500);
});