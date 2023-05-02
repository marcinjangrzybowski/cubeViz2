
function logToDiv(text) {
  // Create a new div element
  const div = document.createElement('div');

  // Set the inner text of the div to the input string
  div.innerText = text;

  // Apply the necessary styles to the div

  div.style.backgroundColor = 'rgba(255, 255, 255, 0.8)';
  div.style.border = '1px solid black';
  div.style.padding = '8px';
  div.style.margin = '4px';
  div.style.fontFamily = 'monospace';
  div.style.fontSize = '12px';

  // Append the div to the body
    document.getElementById("logDiv").appendChild(div);
    return div;
}

	function renderCheckboxesForObject(obj, parentNode) {
		// Function to set all other checkboxes to false except for the target one
		function setOnlyThisVisible(targetKey) {
			for (const key in obj) {
				if (obj.hasOwnProperty(key) && obj[key].hasOwnProperty("visible")) {
					obj[key].visible = key === targetKey;
				}
			}
		}

		// Create a button to set all fields to true
		const allVisibleButton = document.createElement("button");
		allVisibleButton.textContent = "Set All Fields to True";
		allVisibleButton.addEventListener("click", function () {
			for (const key in obj) {
				if (obj.hasOwnProperty(key) && obj[key].hasOwnProperty("visible")) {
					obj[key].visible = true;
				}
			}
		});

		// Add the allVisibleButton to the parentNode
		parentNode.appendChild(allVisibleButton);
		parentNode.appendChild(document.createElement("br"));

		// Iterate through each property in the object
		for (const key in obj) {
			if (obj.hasOwnProperty(key) && obj[key].hasOwnProperty("visible")) {
				const label = document.createElement("label");
				label.textContent = key;

				const checkbox = document.createElement("input");
				checkbox.type = "checkbox";
				checkbox.name = key;
				checkbox.checked = obj[key].visible;

				checkbox.addEventListener("change", function () {
					obj[key].visible = this.checked;
				});

				// Create a small button to set only this field's value to true
				const setOnlyThisVisibleButton = document.createElement("button");
				setOnlyThisVisibleButton.textContent = "Set Only This Field to True";
				setOnlyThisVisibleButton.addEventListener("click", function () {
					setOnlyThisVisible(key);
				});

				parentNode.appendChild(checkbox);
				parentNode.appendChild(label);
				parentNode.appendChild(setOnlyThisVisibleButton);
				parentNode.appendChild(document.createElement("br"));
			}
		}
	}

    function main(cvd,session,refSpace) {
        // Get A WebGL context
        /** @type {HTMLCanvasElement} */
        var canvas = document.querySelector("#canvas");
        var sliceSlider = document.querySelector("#slice");
        var gl = canvas.getContext("webgl2",{ xrCompatible: true });
        if (!gl) {
            return;
        }
        var termPre = document.querySelector("#term");
        var termPreMap = document.querySelector("#termMap");


        // setup GLSL program
        var program = webglUtils.createProgramFromSources(gl, [vs, fs]);
	var programCursor = webglUtils.createProgramFromSources(gl, [vsCursor, fsCursor]);
	var programCodeView = webglUtils.createProgramFromSources(gl, [vsCodeView, fsCodeView]);
        gl.useProgram(program);
        const uLoc = {};
        const aLoc = {};
        const uLocCursor = {};
        const aLocCursor = {};
        const uLocCodeView = {};
        const aLocCodeView = {};


        const getLoc = function (n) {
            aLoc[n] = gl.getAttribLocation(program, n);
        }
        const attrs = ["vPosition", "Normal", "Color", "Mode", "VisFlagF"];
        attrs.map(getLoc);


        const getULoc = function (n) {
            uLoc[n] = gl.getUniformLocation(program, n);
        }

        const unis = ["euler", "screen", "shade", "scaleG", "screenDelta", "VisF", "slice","poseMat","projMat","modelMat","alphaOverwrite","u_textureMap","coCu","uTime","cellCC"];
        unis.map(getULoc);

        //forCursor
        gl.useProgram(programCursor);
        const getLocC = function (n) {
            aLocCursor[n] = gl.getAttribLocation(programCursor, n);
        }
        const attrsC = ["vPosition","vColor"];
        attrsC.map(getLocC);


        const getULocC = function (n) {
            uLocCursor[n] = gl.getUniformLocation(programCursor, n);
        }

        const unisC = ["poseMat","projMat","cuMat"];
        unisC.map(getULocC);

	        //forCodeView
        gl.useProgram(programCodeView);
        const getLocCV = function (n) {
            aLocCodeView[n] = gl.getAttribLocation(programCodeView, n);
        }
        const attrsCV = ["vPosition"];
        attrsCV.map(getLocCV);


        const getULocCV = function (n) {
            uLocCodeView[n] = gl.getUniformLocation(programCodeView, n);
        }

        const unisCV = ["poseMat","projMat","modelMat","u_texture","u_textureMap","coCu","uTime"];
        unisCV.map(getULocCV);


		const cells = {};

    function setOnlyThisVisible(targetKey) {
			for (const key in cells) {
				if (cells.hasOwnProperty(key) && cells[key].hasOwnProperty("visible")) {
					cells[key].visible = key === targetKey;
				}
			}
		}

    function toggleVisibility(targetKey) {
	if (targetKey in cells) {
				
					cells[targetKey].visible = !cells[targetKey].visible;
				
			}
		}


        //codeView setup

	var codeViewVtxBuf = gl.createBuffer(); 
        gl.bindBuffer(gl.ARRAY_BUFFER,  codeViewVtxBuf);
        
	let pPt00 = [0.0 , 0.0 , 0.0];
	let pPt01 = [0.0 , 1.0 , 0.0];
	let pPt10 = [0.5 , 0.0 , 0.0];
	let pPt11 = [0.5 , 1.0 , 0.0];
	
	
	var codeViewBuffData = new Float32Array(
	    [//0.0 , 0.0 , 0.0 ,
		pPt00 , pPt01 , pPt11 ,
		pPt00 , pPt11 , pPt10

	     //0.0 , 0.0 , 1.0 ,
	    ].flat());
	// console.log(cursorBuffData);
	gl.bufferData(gl.ARRAY_BUFFER,
                codeViewBuffData,
                      gl.STATIC_DRAW);
	var texture = gl.createTexture();
	gl.bindTexture(gl.TEXTURE_2D, texture);
	gl.activeTexture(gl.TEXTURE0);

		// Fill the texture with a 1x1 blue pixel.
		gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, 1, 1, 0, gl.RGBA, gl.UNSIGNED_BYTE,
			      new Uint8Array([0, 0, 255, 255]));

	var textureMap = gl.createTexture();

	gl.activeTexture(gl.TEXTURE1);
	gl.bindTexture(gl.TEXTURE_2D, textureMap);
		// Fill the texture with a 1x1 blue pixel.
		gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, 1, 1, 0, gl.RGBA, gl.UNSIGNED_BYTE,
			      new Uint8Array([0, 255, 0, 255]));

	initCodeViewDraw = function(){
	    var size = 3;          // 2 components per iteration
	  var type = gl.FLOAT;   // the data is 32bit floats
	  var normalize = false; // don't normalize the data
	  var stride = 0;        // 0 = move forward size * sizeof(type) each iteration to get the next position
	    var offset = 0;        // start at the beginning of the buffer
	    	    


	   // gl.bindVertexArray(vao);

	    gl.bindBuffer(gl.ARRAY_BUFFER, codeViewVtxBuf);
	    gl.activeTexture(gl.TEXTURE0);
	    gl.bindTexture(gl.TEXTURE_2D, texture);
            
           gl.activeTexture(gl.TEXTURE1);
	    gl.bindTexture(gl.TEXTURE_2D, textureMap);

	  gl.vertexAttribPointer(
	      aLocCodeView["vPosition"], size, type, normalize, stride, offset);
	    gl.enableVertexAttribArray(attrsCV["vPosition"]);
	    
	};

	//codeView setup End
	
        var cursorVtxBuf = gl.createBuffer(); 
        gl.bindBuffer(gl.ARRAY_BUFFER, cursorVtxBuf);
        let pPtTrans = function(p){
	    const a = 0.3;
	    return [a*(p[0]-1.0)+1.0,a*(p[1]+1.0)-1.0,a*(p[2]-1.0)+1.0];
	  };
	let pPt0 = [0.0 , 0.0 , 0.0];
	let pPtX = pPtTrans([1.0 , 0.0 , 0.0]);
	let pPtY = pPtTrans([0.0 , -1.0 , 0.0]);
	let pPtZ = pPtTrans([0.0 , 0.0 , 1.0]);

	let po0Col = [1.0,0.3,0.3]
        let poACol = [0.3,0.3,0.3]

	let zipPt = function (p,c){
	    return p.concat(c); 
	    // [p[0],c[0],p[1],c[1],p[2],c[2]];
	}
	
	var cursorBuffData = new Float32Array(
	    [//0.0 , 0.0 , 0.0 ,
		zipPt (pPtX , poACol) ,
		zipPt (pPtY , poACol) ,
		zipPt (pPtZ , poACol) ,

                zipPt (pPt0 , po0Col) ,
		zipPt (pPtY , poACol) ,
		zipPt (pPtZ , poACol) ,

		zipPt (pPtX , poACol) ,
		zipPt (pPt0 , po0Col) ,
		zipPt (pPtZ , poACol) ,

		zipPt (pPtX , poACol) ,
		zipPt (pPtY , poACol) ,
		zipPt (pPt0 , po0Col)
	     //0.0 , 0.0 , 1.0 ,
	    ].flat());
	// console.log(cursorBuffData);
	gl.bufferData(gl.ARRAY_BUFFER,
                cursorBuffData,
                      gl.STATIC_DRAW);
	


	initCoursorDraw = function(){
	    var size = 3;          // 2 components per iteration
	  var type = gl.FLOAT;   // the data is 32bit floats
	  var normalize = false; // don't normalize the data
	  var stride = 24;        // 0 = move forward size * sizeof(type) each iteration to get the next position
	    var offset = 0;        // start at the beginning of the buffer
	    	    


	   // gl.bindVertexArray(vao);

	   gl.bindBuffer(gl.ARRAY_BUFFER, cursorVtxBuf);

	  gl.vertexAttribPointer(
	      aLocCursor["vPosition"], size, type, normalize, stride, offset);
	    gl.enableVertexAttribArray(attrsC["vPosition"]);
	    
	  gl.vertexAttribPointer(
	      aLocCursor["vColor"], size, type, normalize, stride, 12);
	  gl.enableVertexAttribArray(attrsC["vColor"]);
	};

	gl.useProgram(program);
        const initiated = cvd.webGlDescriptors.map(function (d) {
	    
            var buf = gl.createBuffer();
            gl.bindBuffer(gl.ARRAY_BUFFER, buf);
            var buffData = new Float32Array(d.dvertexData);
            gl.bufferData(gl.ARRAY_BUFFER,
                buffData,
                gl.STATIC_DRAW);
            var f = new Function("program", "webgl", "w", "h", "uLoc", "aLoc", d.dInitCommands
                + d.dDrawCommands
            );



			d.dAddrMap.forEach(function (cell, cellK) {
				const cellAddr = cell[0];
				const fstV = cell[1][0][0];
			    const countV = cell[1][0][1];
			    let kNum = cellK*10; 
                            let ccc = new Float32Array(
				[(kNum) % 256,(kNum>>8) % 256,(kNum>>16) % 256]);
			    cells[cellAddr] = {visible : true ,
					       center : new Float32Array(cell[1][1].concat([1.0])),
					       cellK : cellK,
					       cellColorCode : ccc,
					       codePt : undefined
					      };

			});



            return {buf: buf, f: f, d: d, buffData: buffData};


        });

	const addressClickTest = function (cMat){
	    let testedPt = m4.transformPoint(cMat,new Float32Array([0,0,0,1.0]));
	    let nearestAddr = Object.keys(cells).map(function(addr){
		let cntr = cells[addr].center;
		if(cntr.length == 4){
                    let pC = m4.transformPoint(modelMat,cells[addr].center);
		    return {caddr:addr , dst : m4.distanceSq(pC,testedPt)};
		}

	    }).sort(function(a,b){ return (a.dst-b.dst); })[0].caddr;
	    return nearestAddr;

	};

	

		// renderCheckboxesForObject(cells,document.getElementById("cellCntrl"));


        let vpAlpha = 0;
        let vpBeta = 0;
        let vpGamma = 0;
        let vpScale = 1.0;
        let cfft = new Float32Array([0]);
        let codeCoursor = new Float32Array([
            0.75 , 0.23
	]);

	
        let modelMat = new Float32Array([
            1.0 , 0.0 , 0.0 , 0.0 ,
	    0.0 , 1.0 , 0.0 , 0.0 ,
	    0.0 , 0.0 , 1.0 , 0.0 ,
	    0.0 , 0.0 , 0.0 , 1.0
	]);

	let codeMat = new Float32Array([
            1.0 , 0.0 , 0.0 , 0.0 ,
	    0.0 , 1.0 , 0.0 , 0.0 ,
	    0.0 , 0.0 , 1.0 , 0.0 ,
	    0.0 , 0.0 , 0.0 , 1.0
	]);

        let VisF = 1;
	let selectedAddress = "";

        let sliceVal = 0.5;
        if (cvd.exprDim == 2) {

            vpAlpha = 0;
            vpBeta = 0;
            vpGamma = 0;
            vpScale = 0.8;

        }

	const renderCode = function(){
           
        
	// window.dti = function(){
        
	    const renderF = function(termElem,texture){
	        termElem.style.display = "block";
                
	        domtoimage.toPng(termElem,{height:2048,width:1024})
		.then(function (dataUrl) {
		    // console.log(termPre);
	        termElem.style.display = "none";
		var img = new Image();

		// img.style.position = "fixed";
		// img.style.top = "0";
		// img.style.left = "0";
		// img.style.zIndex = "999999";
		    // document.body.appendChild(img);
		    // Create a texture.



		// Asynchronously load an image
		img.addEventListener('load', function() {
		  // Now that the image has loaded make copy it to the texture.
		    gl.bindTexture(gl.TEXTURE_2D, texture);
		    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR);
                    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
		    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA , gl.RGBA,gl.UNSIGNED_BYTE, img);
		  gl.generateMipmap(gl.TEXTURE_2D);
		});
		    		img.src = dataUrl;
		})
	    
	    .catch(function (error) {
		console.error('domtoimage - oops, something went wrong!', error);
	    });
	    };

	    renderF(termPre,texture);
	    renderF(termPreMap,textureMap);
	};

	termPre.innerHTML = cvd.exprString;
	termPreMap.innerHTML = cvd.exprString;
	termPreMap.style.display = "block"
	renderCode();
	
        // };

	window.rc = renderCode();


        document.querySelectorAll("#termMap .addrWrapper").forEach(function(x){
          const addr = x.id.slice(10);

          if(addr in cells){
            
              let ccc = cells[addr].cellColorCode;
	      let color = "rgba("+ccc.join(",")+")";
	      x.style.color = color;
	      x.style.backgroundColor = color;

              let rect = x.getBoundingClientRect();
	      let codePt = new Float32Array([((rect.left+rect.right)/2)/1024 , ((rect.top+rect.bottom)/2)/2048]); 
	      cells[addr].codePt = codePt;
	      
          }else{
                     // console.log("notIn",addr);
          }
        });


        document.querySelectorAll("#term .addrWrapper").forEach(function(x){
          const addr = x.id.slice(10);

          if(addr in cells){
            x.classList.add("hasCells");
             // console.log("isIn",addr);
             x.addEventListener("click",function(e){

               if (e.shiftKey){
                 e.preventDefault();
                  cells[addr].visible = !(cells[addr].visible);
               }else {
                  setOnlyThisVisible(addr);
               }

           })
          }else{
                     // console.log("notIn",addr);
          }
        });

	
        const toggleVisGroup = function (k) {
            VisF = (VisF ^ 1 << (4 + k));
//	console.log(VisF);
        };

        [1, 2, 3, 4, 5, 6, 7, 8].forEach(toggleVisGroup);

        // [3,4].forEach(toggleVisGroup);
        // vpAlpha = 0;
        // toggleVisGroup(7);

        sliceSlider.addEventListener("input", function (e) {
            sliceVal = parseFloat(e.target.value) / 100;
        });

        function handleScroll(event) {
            // Get the scroll delta
            const delta = Math.sign(event.deltaY);

            // Update the vpScale (zoom) according to the scroll direction
            if (delta < 0) {
                // Zoom in
                vpScale *= 1.1;
            } else {
                // Zoom out
                vpScale *= 0.9;
            }

            // Limit the zoom level to a reasonable range
            vpScale = Math.max(0.5, Math.min(vpScale, 10));

        }

	let dragStartPos = null;
	let dragStartMatrix = null;
	
	let draggingSource = null;

	
session.addEventListener("squeezestart", onSqueezeEvent);
session.addEventListener("squeeze", onSqueezeEvent);
session.addEventListener("squeezeend", onSqueezeEvent);


	
function onSqueezeEvent(event) {
    let source = event.inputSource;

  let targetObj = null;

  if (source.targetRayMode !== "tracked-pointer") {
    return;
  }

  let targetRayPose = event.frame.getPose(source.targetRaySpace, refSpace);
  if (!targetRayPose) {
    return;
  }

    let pos = targetRayPose.transform.position;
    let posArr = new Float32Array([pos.x , pos.y, pos.z]);
  switch(event.type) {
    case "squeezestart":
      dragStartPos = posArr;
      draggingSource = source;
      dragStartMatrix = new Float32Array(modelMat);
      break;
    // case "squeeze":
    //   myDropObject(targetObj, targetRayPose.matrix);
    //   break;
    case "squeezeend":

      
      dragStartMatrix = null;
      dragStartPos = null;
      draggingSource = null;
      break;
  }
}
	
// Add the event listener for the 'wheel' event on the canvas DOM element
        canvas.addEventListener('wheel', handleScroll, {passive: true});

        document.body.addEventListener("keydown", function (e) {
            if (e.code.slice(0, 5) == "Digit") {
                let k = parseInt(e.code.slice(5, 6));
                if (e.shiftKey) {


                    VisF = 1;

                }
                toggleVisGroup(k);
            }
            console.log(e.code);
            if (e.code == "KeyC") {
                document.body.classList.toggle("showCode");
            }
        });


        canvas.addEventListener("mousemove", function (e) {
            if (e.buttons == 1) {
                vpAlpha =
                    Math.min(Math.max(vpAlpha + (((e.movementY) / 1024) * -0.3), 0), 0.5);

                vpGamma += (e.movementX / 1024) * 0.3;
            }

        });
        let fN = 0;
        gl.enable(gl.DEPTH_TEST);
	const drawCursor = function(){


	    initCoursorDraw();
            
            gl.uniformMatrix4fv(uLocCursor["poseMat"], false,  poseMat);
            gl.uniformMatrix4fv(uLocCursor["projMat"], false,  projMat);
            gl.uniformMatrix4fv(uLocCursor["cuMat"], false,  cuMat);
	    gl.drawArrays(gl.TRIANGLES
							, 0
							, 12
					)
	}

	const drawCodeView = function(){


	    
	    initCodeViewDraw();
            
            gl.uniformMatrix4fv(uLocCodeView["poseMat"], false,  poseMat);
            gl.uniformMatrix4fv(uLocCodeView["projMat"], false,  projMat);
            gl.uniformMatrix4fv(uLocCodeView["modelMat"], false,  codeMat);
	    gl.uniform2fv(uLocCodeView["coCu"], codeCoursor);
	    gl.uniform1fv(uLocCodeView["uTime"], cfft);

	    
	    gl.uniform1i(uLocCodeView["u_texture"], 0);  // texture unit 0
            gl.uniform1i(uLocCodeView["u_textureMap"], 1);  // texture unit 1

	    
	    gl.drawArrays(gl.TRIANGLES
							, 0
							, 6
					)
	}

        const lineWRange = gl.getParameter(gl.ALIASED_LINE_WIDTH_RANGE);
        gl.lineWidth(lineWRange[1]);
	
        const drawPrimitives = function (i) {
            if (i.d.dElemNum < 1) return;
            gl.bindBuffer(gl.ARRAY_BUFFER, i.buf);
            i.f(program, gl, gl.canvas.clientWidth, gl.canvas.clientHeight, uLoc, aLoc);

            let vMat = [(vpAlpha * 360)
                , (vpBeta * 360)
                , (vpGamma * 360)
                , (vpScale)];

            gl.uniform4fv(uLoc["euler"], vMat);
            gl.uniformMatrix4fv(uLoc["poseMat"], false,  poseMat);
            gl.uniformMatrix4fv(uLoc["projMat"], false,  projMat);
	    gl.uniformMatrix4fv(uLoc["modelMat"], false,  modelMat);
            // if(fN%30==0)
            // {console.log(poseMat.join("|"))};
            gl.uniform1fv(uLoc["VisF"], [VisF]);

	    gl.uniform2fv(uLoc["coCu"], codeCoursor);
	    gl.uniform1fv(uLoc["uTime"], cfft);
	    
	    
	  
            gl.uniform1i(uLoc["u_textureMap"], 1);  // texture unit 1

            //console.log(i.d.dElemNum, i.buffData.length);
            //    gl.drawArrays(gl[i.d.dPrimitiveMode.toUpperCase()], i.d.StartIndex,
            // i.d.dElemNum
            //    )


            i.d.dAddrMap.forEach(function (cell, cellK) {
                const cellAddr = cell[0];
                const fstV = cell[1][0][0];
                const countV = cell[1][0][1];


		gl.uniform3fv(uLoc["cellCC"], cells[cellAddr].cellColorCode);

                if(cells[cellAddr].visible) {
          	    gl.uniform1fv(uLoc["alphaOverwrite"], [1.0]);
		}else{
		    return;
		    gl.uniform1fv(uLoc["alphaOverwrite"], [0.3]);	    
		}

	        gl.drawArrays(gl[i.d.dPrimitiveMode.toUpperCase()]
				, fstV
				, countV
		)


            });

            gl.uniform1fv(uLoc["slice"], [sliceVal]);
        }
        let lastFrameTime = 0;
	let firstFrameTime = -1;
        let poseMat, projMat , cuMat;

        const clickedState = {right:{},left:{}};

	var doAfterDraw = null;
        const draw = function (currentFrameTime,frame) {
	    if(firstFrameTime==-1)
	    {firstFrameTime = currentFrameTime}
	    cfft[0] = currentFrameTime;

            let viewerPose = frame.getViewerPose(refSpace);
	    
            for (const source of frame.session.inputSources) {
                  const gp = source.gamepad;
                if (gp) {
		    let constPart = 0.0002;
		    let speed = 0.02;
		    let curve = 4;
                    let curveFn = function(x){
		        return (constPart +
			    (1.0-constPart)*Math.pow(x,curve))*Math.sign(x);
		    };
		    
		    
                    if (gp.axes[3] !== 0) {
                        let dy = curveFn(gp.axes[3])*speed;
			 // Math.pow(gp.axes[3],curve)*speed*Math.sign(gp.axes[3]);
			codeCoursor[1] = Math.min(1.0 , Math.max(0.0 , codeCoursor[1] + dy));  
			// vpScale *= 1+gp.axes[3]*0.01;

		    }
		    if (gp.axes[2] !== 0) {
                        let dx = curveFn(gp.axes[2])*speed*2;
			codeCoursor[0] = Math.min(1.0 , Math.max(0.0 , codeCoursor[0] + dx));  
			// vpScale *= 1+gp.axes[3]*0.01;

		    }
		    // if (gp.axes[3] !== 0) {

		    // 	vpScale *= 1+gp.axes[3]*0.01;

		    // }
		    // if (source.handedness == "right") {
                    //     if(madeClickTest && gp.buttons[0].value== 0){
                    //         madeClickTest = false;
		    // 	    makeClickTest = false;
		    // 	} else if(!madeClickTest && gp.buttons[0].value!== 0) {
		    //         makeClickTest = true;
		    // 	 }
                    // }

                    gp.buttons.forEach(function(b,i){
			if(clickedState[source.handedness])
			{
			    var p = b.value!=0 || b.pressed;
			    var hnd = source.handedness.slice(0,1);
                            if(p && !clickedState[source.handedness][i]) {
                                clickedState[source.handedness][i] = true;
				// logToDiv("on "+String(i));
                                if(hnd == "r" && i == 5){
               			// let targetRayPose = frame.getPose(source.targetRaySpace, refSpace);
				//    let addr = (addressClickTest(targetRayPose.transform.matrix));

				//     // setOnlyThisVisible(addr);
				//     // toggleVisibility(addr);
				//     selectedAddress = addr;
				//     // doAfterDraw = renderCode;
				//     if(cells[addr] && cells[addr].codePt){
				// 	codeCoursor.set(cells[addr].codePt);
				//     }

					
				}
				if(hnd == "l" && i == 5)
				{
				    
                                    // termPre.innerHTML = String(fN);
				    // renderCode();
				}
				
			    }else if(!p && clickedState[source.handedness][i]){
                                clickedState[source.handedness][i] = false;
				// logToDiv("off "+String(i));
			    }
			}

			if(p){
                            if(hnd == "r" && i == 4)
			    {
				 let targetRayPose = frame.getPose(source.targetRaySpace, refSpace);
			          
				    codeMat.set(targetRayPose.transform.matrix);
			    }

			    if(hnd == "r" && i == 5)
			    {

			    let targetRayPose = frame.getPose(source.targetRaySpace, refSpace);
				   let addr = (addressClickTest(targetRayPose.transform.matrix));

				    selectedAddress = addr;

				    if(cells[addr] && cells[addr].codePt){
					codeCoursor.set(cells[addr].codePt);
				    }
			    }


			}
			

		    });

		    
		    
		    // if(source.handedness == "right" && gp.buttons[0].value!== 0
		    //     && draggingSource===null){
		    // 	let targetRayPose = frame.getPose(source.targetRaySpace, refSpace);
		    // 	// madeClickTest = true;
		    // 	// makeClickTest = false;
		    // 	// vpScale *= 1.5;
		    // 	let addr = (addressClickTest(targetRayPose.transform.matrix));

		    // 	// setOnlyThisVisible(addr);
		    // 	toggleVisibility(addr);
		    // 	logToDiv(addr);
		    // }

		    
                      //   else if (gp.axes[2] !== 0) {
                      //   vpScale *= 1.01;
                    // }
                    if(draggingSource==source){
			    let targetRayPose = frame.getPose(source.targetRaySpace, refSpace);

			    let pos = targetRayPose.transform.position;
                            let posArr = new Float32Array([pos.x , pos.y, pos.z]);
	                let dV = m4.subtractVectors(posArr,dragStartPos);
			    m4.translate(
				dragStartMatrix ,
				dV[0],dV[1],dV[2],
				modelMat);
		    }

                  }
                }
            if (viewerPose) {
              const glLayer = session.renderState.baseLayer;


              // Start by erasing the color and depth framebuffers.
              gl.bindFramebuffer(gl.FRAMEBUFFER, glLayer.framebuffer);
                  
              gl.clearColor(0, 0, 0, 0.0);
              gl.clearDepth(1.0);
              gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

              // Compute the time elapsed since the last frame was rendered.
              // Use this value to ensure your animation runs at the exact
              // rate you intend.

              const deltaTime = currentFrameTime - lastFrameTime;
              lastFrameTime = currentFrameTime;

              // Now call the scene rendering code once for each of
              // the session's views.

              for (const view of viewerPose.views) {
                const viewport = glLayer.getViewport(view);


                // console.log(viewport.x, viewport.y, viewport.width, viewport.height);
                poseMat =  view.transform.inverse.matrix;

                projMat =  view.projectionMatrix;
		  gl.useProgram(program);
		  gl.viewport(viewport.x, viewport.y, viewport.width, viewport.height);

		  
		  initiated.forEach(drawPrimitives);
                  
		  
		  window.iscs = frame.session.inputSources;
		  for (const source of frame.session.inputSources) {
                         const gp = source.gamepad;
                      if (gp) {
			  let targetRayPose = frame.getPose(source.targetRaySpace, refSpace);
			  cuMat =  targetRayPose.transform.matrix;
			  // if (gp.buttons[1].value !== 0) {
                          //     modelMat = cuMat;
                          // }
			  
			  // vpScale *= 1.0001;
			  // uMat =  view.transform.inverse.matrix;
                          gl.useProgram(programCursor);
			  drawCursor();

                  }
                  }
		  	 gl.useProgram(programCodeView);
			  drawCodeView();

              }
            }


        }
        gl.clearColor(0, 0, 0, 0)
        // gl.clear(gl.COLOR_BUFFER_BIT)
        //gl.cullFace(gl.BACK);
        //draw();


        function step(timestamp,frame) {
            fN++;
            // console.log(frame);

            draw(timestamp,frame);
	    if(doAfterDraw){
		doAfterDraw();
		doAfterDraw = null;
	    }
            session.requestAnimationFrame(step);

        }
        session.updateRenderState({
            baseLayer: new XRWebGLLayer(session, gl),
          });
        console.log("firstFrameReq");
        session.requestAnimationFrame(step);

    }

    function createPreTag(content) {
        // Create a new <pre> element
        const preElement = document.createElement('pre');

        // Set the content of the <pre> element
        preElement.textContent = content;

        // Append the <pre> element to the body
        document.body.appendChild(preElement);
    }


    if (cvdR.Right) {
      // const sesionTy = "immersive-vr"
	const startSession = function(){



	    
        if (navigator.xr) {
          // If the device allows creation of exclusive sessions set it as the
            // target of the 'Enter XR' button.

            function startSesionOfType(sesionTy){
	     navigator.xr.requestSession(sesionTy).then((session) => {
                  console.log(session);
                  document.body.removeEventListener("click",startSession);
                  session.requestReferenceSpace("local").then((refSpace) => {
                    console.log(refSpace);
                    // if (session.domOverlayState) {
                    //   logToDiv("session.domOverlayState.type");
                    // } else {
                    //   logToDiv("DOM overlay not supported or enabled!");
                    // }

                    main(cvdR.Right,session,refSpace);

                  });

                /* continue to set up the session */
              }
						       )
	    }
	    let supportedModes = [];
          navigator.xr.isSessionSupported("immersive-ar").then((supported) => {
            if(supported){
		supportedModes.push("immersive-ar");
            }
                 navigator.xr.isSessionSupported("immersive-vr").then((supported) => {
		     if(supported){
			 		supportedModes.push("immersive-vr");
		      
		     }
		     supportedModes.forEach(function(m){
			 var b = logToDiv("ready: "+m);
			 
                         b.addEventListener("click",function(){
			     startSesionOfType(m);
			 });
		     });
		     
		});
	    
          });
        }


	    ;}
	startSession();
      // document.body.addEventListener("click",startSession);


    } else {

        createPreTag(cvdR.Left);
    }
