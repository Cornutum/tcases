
function selectTestCase( functionName, testCaseNum) {
    var i;
    var functions = document.querySelectorAll( ".function");
    for( i = 0; i < functions.length; i++) {
        functions[i].style.display = "none";
    }
    var fdiv = document.getElementById( functionName);
    var testCases = fdiv.querySelectorAll( ".testCase");
    for( i = 0; i < testCases.length; i++) {
        testCases[i].style.display = "none";
    }
    document.getElementById( functionName + "." + testCaseNum).style.display = "block";
    fdiv.style.display = "block";
}

function makeSelectTestCase( functionName, testCaseNum) {
    return function() {
        selectTestCase( functionName, testCaseNum);
        return false;
    };
}

var tcmenu = document.createElement( "DIV");
tcmenu.id = "testCases";
tcmenu.style.position = "absolute";
tcmenu.style.top = "0em";
tcmenu.style.left = "0em";
tcmenu.style.width = "16em";
tcmenu.style.borderStyle = "solid";
tcmenu.style.borderColor = "#000000";
tcmenu.style.borderWidth = "1";
tcmenu.style.margin = "1em";
tcmenu.style.padding = "1em";
tcmenu.style.overflow = "auto";
document.body.appendChild( tcmenu);

var fdivs = document.querySelectorAll( "div.function");
var i;
for( i = 0; i < fdivs.length; i++) {
    var fdiv = fdivs[i];
    fdiv.style.position = "absolute";
    fdiv.style.top = "0em";
    fdiv.style.left = "20em";

    var fname = document.createElement( "H4");
    fname.appendChild( document.createTextNode( fdiv.id));
    tcmenu.appendChild( fname);

    var tclist = document.createElement( "UL");
    var tcdivs = fdiv.querySelectorAll( ".testCase.success");
    var j;
    for( j = 0; j < tcdivs.length; j++) {
        var tcid = tcdivs[j].id.split(".")[1];
        var tclink = document.createElement( "A");
        tclink.innerHTML = "Test Case " + tcid;
        tclink.href = "#";
        tclink.onclick= makeSelectTestCase( fdiv.id, tcid);

        var tcitem = document.createElement( "LI");
        tcitem.appendChild( tclink);
        tclist.appendChild( tcitem);
    }

    var fcdivs = fdiv.querySelectorAll( ".testCase.failure");
    if( fcdivs.length > 0) {
        var fclist = document.createElement( "UL");
        for( j = 0; j < fcdivs.length; j++) {
            var fcid = fcdivs[j].id.split(".")[1];
            var fclink = document.createElement( "A");
            fclink.innerHTML = "Test Case " + fcid;
            fclink.href = "#";
            fclink.onclick= makeSelectTestCase( fdiv.id, fcid);

            var fcitem = document.createElement( "LI");
            fcitem.appendChild( fclink);
            fclist.appendChild( fcitem);
        }

        var tcitem = document.createElement( "LI");
        tcitem.appendChild( document.createTextNode( "Failures"));
        tcitem.appendChild( fclist);
        
        tclist.appendChild( document.createElement( "P"));
        tclist.appendChild( tcitem);
    }
    
    tcmenu.appendChild( tclist);
}

selectTestCase( fdivs[0].id, 0);
