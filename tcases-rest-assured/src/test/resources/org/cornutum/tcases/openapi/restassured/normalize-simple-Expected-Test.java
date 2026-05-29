package org.cornutum.tcases.openapi.restassured;


import org.junit.Test;

import java.util.List;
import java.util.Map;
import static java.util.stream.Collectors.*;

import io.restassured.http.Header;
import io.restassured.response.Response;

import org.cornutum.tcases.openapi.test.ResponseValidator;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class NormalizeSimpleTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void getHeaderArray_EmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "-663983632")
                .header( "nullable", "")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "-973846444,-491570251,248226525")
                .header( "empty", "-763005655")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyItemsSize_Is_Gt_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", ",397454609,-419034757,1016002009,-806053607,-184138374,912373594,-387503082,273418105,-14295134,366879966,-417170173,-54988136,-957016506,978259428")
                .header( "nullable", "")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", ",202760021,346377702,-562599694,471203691,597877542,-740518727,-775657728,346377702,-478776843,423048701,878470621")
                .header( "empty", ",-263561408,-753330793,-668885281,3441147,525139564")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyItemsContainsValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "-1000663847")
                .header( "nullableNonEmpty", "-581105902,-438753666,-275148878,890213106,-515357008,-388869290")
                .header( "nonEmpty", "0,752661889,63645493")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "955768608,199668738,-149726808,955768608,464311934,-593573581,-7441495,41073901,-986667692,447238487")
                .header( "nullable", ",140398850,550313950,-993640878,-868406336")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "237300151,1066094687,850828722,968356660,611041745,-46633057,-663248175,-347183716,153341441,-845832722,-845832722,-474473431")
                .header( "empty", "508946787,-732631463,508946787,-257635928,56434952,-32893848,1068193310,-809792451")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableItemsContainsValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "-620549958")
                .header( "nullable", "0")
                .header( "nullableNonEmpty", ",-963881102,,-122365687,744427400,-367786345")
                .header( "nonEmpty", "-944635253,-102765888,317107834")
                .header( "empty", "-387197391")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0,26391051,158398104,-397374631,-233062722,434248008,-1008084093")
                .header( "nullable", "702357003,-125362082,-770023321,-125362082,898188270,-251995753,-576287504")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,-870161892,-928978638,133353299,787802039,-77067733,-928978638")
                .header( "empty", "0,755435036,668309383,1050622568,360822738,992634790,672570154,-227048237,-739175557,-661058485,752932548,-911220633,-223425552,987179039,-58668693,196486998")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableNonEmptyItemsContainsValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "120760913")
                .header( "nullable", "")
                .header( "nullableNonEmpty", "0,-488514263,-626178645,60574280,-1037206809,-752193541,1020058403,-523444251,-493466293,80227061,-532488027,413026442")
                .header( "nonEmpty", "176117552,1030711683,9431902")
                .header( "empty", "443349246")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableNonEmptyItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "-272985398,-272985398")
                .header( "nullable", "-792565972")
                .header( "nullableNonEmpty", "406800812,510622585,206988267,406800812,67765969,99929274,395083493")
                .header( "nonEmpty", "-162423683,75024280,-745496012,838720974,-162423683,-502433306,-776927168")
                .header( "empty", "-238095170,-238095170")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,151962706,136580216,285651285,-924039781,782840592,-801001406,237056446,-551448528,-1044669035,383049429,-83479053,343633770")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,-671288562,869604699")
            .when()
                .request( "GET", "/header/array")
            .then()
                // empty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,-627843641,-21461969,-1971442,-603620432,-50652039")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,-70741765,-57893825")
                .header( "empty", "")
            .when()
                .request( "GET", "/header/array")
            .then()
                // empty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,315661103,-577787599,-69360781,223046093")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,1040796428,-309892778")
                .header( "empty", "true")
            .when()
                .request( "GET", "/header/array")
            .then()
                // empty.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,696974517,365527133,76164847,249271296,-956024759")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,438727386,754238690")
                .header( "empty", "hB[-B!iN")
            .when()
                .request( "GET", "/header/array")
            .then()
                // empty.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,637191328,108545445")
                .header( "nullableNonEmpty", "")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,-479549106,-730659460")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NonEmptyType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,844290300,553010241,642195308,687417990")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "-939.0")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nonEmpty.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NonEmptyItemsSize_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,583262582,-796037922,891783364,-14695305,135499191,-474902245,-105974791,-534457303,84437951,-297821477,-70798836")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,0")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nonEmpty.Items.Size=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NonEmptyItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,478047900,1040836124,227772242,-157573268,-993281356,401365385,-173353674,726611935,733730348,807647701,624958094,774215316")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "U,vr,-419361896,-98253235")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nonEmpty.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,948999425,190177970")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nullable.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "931")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,87826848,170335191")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nullable.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "true,-117419213,156302843,-30096860,-318683953,527118070")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,-494542682,929475630")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nullable.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableNonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,234307850,355644044,288570667,-715028690,-931311668,496762735,-916838665,982290292,241480259,-852153679,795287776,938196706,-997893197,1021085818,-442738043")
                .header( "nonEmpty", "0,-496966049,982371918")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nullableNonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableNonEmptyType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,-33686823,1038563943,275793674,437615995,-292598457,-914090369,469603470,61974558,217734896,-238666157,227093318,688968913,761589264,718919277")
                .header( "nullableNonEmpty", "-554")
                .header( "nonEmpty", "0,-431903362,-887034220")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nullableNonEmpty.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableNonEmptyItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,647668931,-1046806958,-937158721,-1051105684,844634866,-205233759")
                .header( "nullableNonEmpty", "true,-43419965,296879203,411937858,1061863112,485393116,-1031149706,-633270249,409822493,-603602259,913851835,124539711")
                .header( "nonEmpty", "0,-426484906,-176246663")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nullableNonEmpty.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_ExplodedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "0,615188794,-392367729,-580021754,-866006732,1002385080,-169374993,320843229,-950695107,110094579,531755763,-1060025538,747004232,210673833,903378404,586385060")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,-36884810,-194473291")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // exploded.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_ExplodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "")
                .header( "nullable", "0,-77159184,-922829226,-973959095,-708765133,778972981")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,386601840,-954316598")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // exploded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_ExplodedType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "154.9")
                .header( "nullable", "0,351244239,542774272,-989036461,813821337,593570391,-567512967,-285113409,564107025,-348089125")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,866603201,-108078389")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // exploded.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_ExplodedItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "")
                .header( "nullable", "0,508596481,-973380565,-290625516,-1054372531,562575495,905670226,252054383")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,29854781,-192194137")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // exploded.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "width=0,height=0,zrmvbb=693")
                .header( "nullable", "")
                .header( "nonEmpty", "width,0,height,0,jhmxn,true,gnija,H")
            .when()
                .request( "GET", "/header/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesWidthDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "width=")
                .header( "nullable", "")
                .header( "nonEmpty", "height,")
            .when()
                .request( "GET", "/header/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesWidthType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=,gyqyveo=-716,etzr=true")
                .header( "nullable", "width,0,height,0,clbppuzo,;&m(,,cq,-621")
                .header( "nonEmpty", "width,,jyuimt,true,braltg,qpi,932,zdlflwwlx,-193.0")
            .when()
                .request( "GET", "/header/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesWidthValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "width=684861679")
                .header( "nullable", "width,")
                .header( "nonEmpty", "width,44248481,height,1036707245")
            .when()
                .request( "GET", "/header/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesWidthValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=108229008,zccpvwduebtkue==$B>G],zWKcuAN,HXT")
                .header( "nullable", "width,527530717,height,,rieymtgvatzbhvo,V,jhjpkl,327.1")
                .header( "nonEmpty", "rjlhibbmvqy,-436,ioixqlaxoius,92.7,rqhmvyt,+,Y7!PDm,zVI#D),*CQ811]")
            .when()
                .request( "GET", "/header/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesHeightValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "width=0")
                .header( "nullable", "height,1000786352")
                .header( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/header/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,xhxzxyyzfltfxs=932.7,fyphnxrx=qhjxe,130.8,dv,true,oknkjy,-110")
                .header( "nullable", "width,0,msqhqjw,q&+7,H,rya,lcwiauizitnkj,-302.4,afkgti,NW,GS3Uuv|,+>B,qxff,614.4")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,oj=true,kgnrgugbvkmjh=true")
                .header( "nullable", "width,0,iuqgjrvtauy,[TjOjW3],baibeftejwi,985")
                .header( "nonEmpty", "")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,rgdfnd=132")
                .header( "nullable", "width,0,aclkfs,138,oj,")
                .header( "nonEmpty", "558")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,vzcay=C'OEd,hehahxmuynlfga=yx^\\,ymv=-307.0")
                .header( "nullable", "width,0,dcm,=uUu7,d,R*,rikuyypjwjvhkkrg,6R:,jkhg,267.6")
                .header( "nonEmpty", "width,,cqnotdimhz,-867,nwsx,-410.7")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,moywwjbmaggtbup=pnuiqvxlhxk,\\c,/[tilC,yq,x}aD?p,nzzjxznm=-515.5")
                .header( "nullable", "width,0,kvifenrldmcbhd,702,lmirgwnhoptllig,")
                .header( "nonEmpty", "width,-1,renlgvuvw,245.0,sccilisyjajrzdf,-338")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,jkuvmfwnz=139.8,iqpkcxvxa=,ezmazfgylekhlu=-110")
                .header( "nullable", "width,0,aouz,true")
                .header( "nonEmpty", "height,-154.6,gciaja,308,lg,of%Zk,6iBwQmk,cei,-625")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,tfoeqb=215.7,tdfncefgrg=p(A3jH`")
                .header( "nullable", "width,0,kgq,)")
                .header( "nonEmpty", "height,-1,bnspo,nao,true,dnkinmsxgtecikj,-933")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,nzqzhne=-509")
                .header( "nonEmpty", "bgsmzum,,qrqtbzenz,mqcfuxolvzu,FC2Sj")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nullable.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,ftzjbgem=-922")
                .header( "nullable", "true")
                .header( "nonEmpty", "zghhbfxhzepz,N,0#3%Af,l,550,c,true")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nullable.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,nkenwfgrxfnebp=,tzbfv=gV4KRNz,,lhlnivpwodyiq=")
                .header( "nullable", "width,.! [1)G,yexvrqtrnzgi,true")
                .header( "nonEmpty", "pwe,zvkzvkznvufmpzi,465.2,yaegs,true")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nullable.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,ihzfldnyhwqrjbnp=true,hndqyd=myetkkpduk,true,sdnmckucprrv,322")
                .header( "nullable", "width,-1,vopxenn,qTG.PnvE,XcpETy2,Luwb?6>,auwrjnjqjte,true")
                .header( "nonEmpty", "mxrpaofavpglhz,{qi,2B39~0[-,qohfameupxqkftp,8.&r")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nullable.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,mnow=")
                .header( "nullable", "width,0,height,XZ:c%,endppmqtg,-122.7")
                .header( "nonEmpty", "hjxhkfhxs,true,grovfsxisohz,true")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nullable.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,vms=")
                .header( "nullable", "width,0,height,-1,w,375,phkezvojqnqnhg,g,kC3T$d,R?],,zlsldjexaerqsx,thriogfu,M\\HJ,mbufloiqdigfk,,9X@BkcI~")
                .header( "nonEmpty", "vbsqbjwnnw,-149,wkvfcihwhdrvys,T,najk,143.8")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nullable.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,0,dfuwnctvyoysgh,2F!h0")
                .header( "nonEmpty", "w,8yD9q:s,gtgguskdrkxzq,-292,r,-240")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "")
                .header( "nullable", "width,0,z,-886,snvscobqxpdkf,true,hfchtojvpmlmbmgz,125.7")
                .header( "nonEmpty", "pzowvcwpddtwmxb,u,F7ci,juvtqyby,-157.1,wcgljiz,pdDmz=d;,U1.K:iA,")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "")
                .header( "nullable", "width,0,aog,,nwvlqswk,271,wutyxoo,")
                .header( "nonEmpty", "yskundofwutzrvl,,rolqwxknlug,true")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "width=ozocmrlhwqbunjo,&#,dlbsku,477,ofrlhytlhw,`,height=0,musyglecx=xgocnsacwjozkf,433,xvmgqibsvowaushz,Z:9m*j\",xzu,-822.1,zswitmyjob=\",jA8y2:p,l,qil=auxgug,-310,ddoyk,891")
                .header( "nullable", "width,0,mftzixf,-947,ggocagzcvsakukg,-305,sr,true")
                .header( "nonEmpty", "yj,-676.8,fipmypwl,965,drhteqbsasdqgkn,=Y%O_")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "width=-1,height=0,to=awplridoxkphdm,-17,dpuny,,dqjoq,-834.2,s=:.,vwioaxdqwsn=:")
                .header( "nullable", "width,0,ipqjyhhmqiplsk,true")
                .header( "nonEmpty", "pgflspc,373.1,hntf,wpjf,,j<6")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=true,croi=s,rfux=true,orpiwdrhh=mhzkb,true,m,-325.4")
                .header( "nullable", "width,0,nlvinmiotogykx,QQ,uldauvvfvnfz,,xallfyfzikucew,")
                .header( "nonEmpty", "pr,959")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=-1,qllua=-840,kmhcccwzzs=true")
                .header( "nullable", "width,0,clc,-1023.2,nrcosxzy,786.5")
                .header( "nonEmpty", "swbxcskj,true,zsyzslyojdnh,pniqwuommzamt,`.L%o'\\k,xuth,383.7,nbphk,-357.2")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_EmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", ",:t\"S`9T\\[450nSg],I=BTHC6;vgsi}{1YsSiTi|>S:c`%\";D'[b&IY0S#=[fBQR|Yk_o#)?iw1sv09rscxxp8%X/#>V|=KN(ip@[:+&rEEMcN.PyrD\\.9M5:</v2\\t|3I;&@@K~$JU@ihst_E[pP)^oQ)x4U1!` `FGJ+1Z~3\"P(Ys|!DI,F) >*$t{egzYY<JB](%E4N9BRTW")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "HzX")
                .header( "empty", "(")
            .when()
                .request( "GET", "/header/string")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_EmptyValueLength_Is_Gt_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "o|HhJW4M.w(:\"QB6?Nu^7M%pKSwr/BFl6m7V$v2vn mLDewSA>qQZ=`bnm^hT=[M~LM]2iAtQ*{M*H< yoM\\Z<]t%QO{7_`&pEdwsXr3Fgmd J| =o_oc=[`Pal#.G`.4{wREEbl'H<q%|EdyDlPp`z<rpJWEwMgaqlhIs6/-b9X9sg~x>>tB%,/k~ c\"V_Ak1T=Jco~")
                .header( "empty", "D\\X2>_qzE`@c% Kg,)xak\\Q:bcGrj6(F(g8\\'gX'uf8ui~Y+F>-X(mg(f3NDGZW$sU\\l,8dK4Ush*wg5cupL@=wy5R:7{*B}O$/dmfFfe^>quwQw]Cb_@R\"-+\"")
            .when()
                .request( "GET", "/header/string")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_NullableValueLength_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "")
                .header( "nullableNonEmpty", "X(o!+EUV<<>`=CBg2d\"Ow:e6KT6;IUFdOY}vLS&K8^S:F+,*V$`$p.VlD}[!xuGEGh_MV$i:26U'~lb<#Ctwr~#\\d8l= SP^A6>|V@<p idGE`zGK-[#'o/K*#@mPxlw4ejJJBF<m?msi")
                .header( "nonEmpty", "X,S")
                .header( "empty", "R")
            .when()
                .request( "GET", "/header/string")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_EmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "\\FRR`~6E'JU(8+l\"o! G-M3VX_UxHz~rHo&3mshX&U{Bu9J_GI'ekwfY})Fo{JuFa}]]Sd8$&x9_%Bh,Bu3p}9%{Q,2e>=R/-)D`0Q;")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "f_~3+2Dys=ohR+{T8*hW$:%2Z7Udz)VMKq)8$Zc\\DOqfvFSDLh@^8BwBC)q*6(H](E4iEMT205M.w2pMI;$BwM\"4-/cp^f1W<R,',~~6E'{_>(ZTh_wt'k_.uo}b#<`j^P@xz&}?\"e,XS$plN ~[0E(:bVT}nN+D8~L7Ni'\\|_}g7N@:04p}o)\"&0 V|_3bYGA\"vho1:\"%13?BvplkOt4ublrE>JSCa+m&{#pK5{JxIb?+TH#eN7PJo\"Y[b-YBNt")
            .when()
                .request( "GET", "/header/string")
            .then()
                // empty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_EmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "~L{[#pc _qpTW]`v^=H{@2TJiVK2,f#x9fs\\z0U^n;3[,h7 -5)L7~N[?fj\"|_n]elp:aA?k5O}NCLA1\"7\"4n'#CeJ@`EJ+{HM/mwM`,.BR0A#!")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "3Y!}Zg4DbmFZ;sjRz4\" X}x3Bpak+~?D:?5j_D&;leD)#wGUu?pED[.7c/\\B~PaT\\:4x7j+(}Tn[?V50smO.%s8!QXwg~b*]ALUF&CH~0V_Pd`-3xbZmws?v2Rpj/f1^i^2iVySVmj8~t++\".1L9Mk_d~2x&q}7Lf19C-A Kg~K4XfU^7i&{<p94(=j4*lDd@=q| o?%]s rPt/*vom*\\V:v/&5fr=P=Wa~s''#q\":CoWP^zw`E)Ss?gt@T")
                .header( "empty", "")
            .when()
                .request( "GET", "/header/string")
            .then()
                // empty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_NonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "z5soZ<Hi|o^EIU!<rs+jt<}sEKs0:]x`Qv$S(P\\Q~Qkr]6L>}DXTz k$O,pt.z<#82?;$URwF$ VpxtPj\"aye\"\\g:hAWO#!{HloWxb$_`[x'0,m}qV$6|@WwWnU;KcSda5m\\l NDt7<jmHGOIwc*Eb+3;L=Axqz`&Q?D%tIK]JG+/:?U/WIyoq\\.U`o'gMllr+uCfq[O5+\\~c!%U~")
                .header( "nullableNonEmpty", "")
                .header( "empty", "kmaNOR+^*I!`zy}pD<,A'3@3h\"|uXQNd8yCU)ow`xSOFxIfrc] 7vY.YGU|Kc'2e)_:uH$clo$xDn`nk$U$&b\"JPJ5YeDxMR%(]necb|EUa[1zWu\\2y$,D_@7&swhL9~o(XbW-U=gv2|**b6;A`zNrty}j!qHxVXZy\\T-qIU=?KXIcQ*)(LRO=;Prs$(7hM+*Y kq>ueSUy ,wr2B*~x_x!ZFAb@QO>sj=I")
            .when()
                .request( "GET", "/header/string")
            .then()
                // nonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "'r?#C|*\\nk\"CS$4]BVTuU@R0Gq8xc}'mJq4o7D;WVKqu*6CV&=,8mrKV{;c+.xCN%\"_}s Fc")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "")
                .header( "empty", "Kg9ok`=Plt6J~_fOgLwQ9IPH4v9}9|G!?Sdkj6i'o$1JrJ96'pyFgS=2'4!1H~Qi.X)p|`2#XkY[5{#_@&\\$<N]K=Q'F4#R*-j\\m\\j")
            .when()
                .request( "GET", "/header/string")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_NonEmptyValueLength_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "WV^l*r$A)K@1r7Wx!hRflRfGvf9X3//C|6h:Ci,~Qyg\"r2:$S)#e3R^Lkg&8<-Xh%=x+\"r)FS/Z* ?o4wol8TB#J>UY]%+~vtHp$FXd`B0g4X|\\{K\\E/&sLj*!I7urAs13./#r#1,io.:N=YWu;b?(Z_cbh.G,MuF0XeE ~#!|/Ak<[~Jf W)HF9%I) `HR$pYUUgI*mIf`Ijesi")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "dz")
                .header( "empty", "v~'Y$rz3[q|BV\"N@~\\/d:}%rbwRqnyV]jkUpTTd'QGW!q TmR@T*9a^NIzCWE)6,yhEX9,l9k6f])m{qle^74zeSR9{aNY4G 2#jtKv37$\\jT,L6KVsJ3YCiZ;57XZP{svzzo`Dt[,:5Mu**Hr#daxwST;zEvc&i`ez/#)nj/`bu_1=")
            .when()
                .request( "GET", "/header/string")
            .then()
                // nonEmpty.Value.Length=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_NullableDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "te}sG^IGe3_[{zNsZ8Xd|c\"i6A-Wx!Rcb;RA-uYS6`u^_3h8q:pK&psPFAIT^CK&Y3{!m1rS.?UlY$Iz|\\t<1'\\dI~;~TL[,l&o<=@2[g't;~ZxbRh1[e9({jKgZpgVB=`7^QG\"8YgUlbB%_-^XuZ!z@+:YY'BEZiq\\2=U")
                .header( "empty", "Mof13'XuWaQn{s$:f% '2[M{Y}@m`Oxe+!vi=prkD6c|i5>6o;;08qwX nAd^\"Q~4Ba;aQ;nmpQ3Z5a^[fZVe|g{+$g8{z'{0a:Ly9[,a.:k#PP', 0.xG*|'IL BRiGvJz}p1DrAn1/{>xN+}:wl mbdP2g_M27%Xj$UdN6D")
            .when()
                .request( "GET", "/header/string")
            .then()
                // nullable.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_NullableNonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "D5.s\\Y[cs'0y<WTh;Q'l4X/#6&tTXg8@-%0~!j{0<\"zk4aDstY<=f7rGg\"9o4l*)(${:vlt]qPJSv}QSHDA\\MrUxp&udXGAhpVOSZHl>eC6*@fu>uDn!fBk{cKcLern*2uI}`CwI.IT(x~m?~bApUOPpy(vyAk(Wf.P1w~j}")
                .header( "nonEmpty", ",t|Z+_Id\"EJ|;\"e8m7BXdPS,y%;j^dIh\\(Wu6%\\O`w/;FG \">Az *h:+ ;5AbQrAzQ[Q%l=sho*qc,xb;JI=jmuTL/6bXSe%Y7&FWAXgDG&]`OZG5x1i{tX}([XvIs6}%'v).p^:")
                .header( "empty", "\" o{FN;fEE95C$FCNMLYlYBc?^M\\n)3J5uesQ4pzZjbL@CYExxBlRGa}u/|`P#~N55r(-O=xzJ`h7s2^</@;!1rXi<#-`^95OIY&>*k.")
            .when()
                .request( "GET", "/header/string")
            .then()
                // nullableNonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-57006231")
                .pathParam( "nullable", "-391102849")
                .pathParam( "nonEmpty", "-443546249,-928868785,-1040191651")
                .pathParam( "empty", "-351194572")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyItemsSize_Is_Gt_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", ",684619882,-915629898,-777592863,-787864396,968195964")
                .pathParam( "nullable", ",594107646,308751563,-290067183,-159033827,-879373065,-998839893,1007387311,840979781,718184765,-719901843")
                .pathParam( "nonEmpty", ",-466899496,608407672,-599674523,357217378,-341008120,-362664233,-341008120,-229865362,-884703973,226034055,449528184,-71653421")
                .pathParam( "empty", ",-279296209,485709141,-216666114,-453603517")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyItemsContainsValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "0")
                .pathParam( "nullable", "0")
                .pathParam( "nonEmpty", "0,-935803584,963242121")
                .pathParam( "empty", "0")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "464352220,-1008257777,247662957,-834887514,206516015,-544659415,532728853,-664928320,826313653,206516015,98214662,-164249220")
                .pathParam( "nullable", "449955037,-476185014,-473321033,-10240067,-858529578,757335484,822581821,-956279549,220628827,-858529578,895300504,-739813784,722180891,170367393")
                .pathParam( "nonEmpty", "23914493,196015947,-955915178,642055191,-529026898,650334394,757117358,191955921,660155146,23914493")
                .pathParam( "empty", "172024609,-483358393,29534561,-210497736,172024609")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-669501885")
                .pathParam( "nullable", "-385609986")
                .pathParam( "nonEmpty", "-229975998,678658038,1022681121")
                .pathParam( "empty", "")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // empty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-777374064")
                .pathParam( "nullable", "-365727516")
                .pathParam( "nonEmpty", "-527133846,772732844,-328103540")
                .pathParam( "empty", "-213.4")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // empty.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-449024820")
                .pathParam( "nullable", "-194716814")
                .pathParam( "nonEmpty", "-978628238,523053929,1050022020")
                .pathParam( "empty", "")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // empty.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-1028647183")
                .pathParam( "nullable", "-745144127")
                .pathParam( "nonEmpty", "")
                .pathParam( "empty", "-467487980")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NonEmptyType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-361020438")
                .pathParam( "nullable", "-621459142")
                .pathParam( "nonEmpty", "true")
                .pathParam( "empty", "-321574503")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NonEmptyItemsSize_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-957197905")
                .pathParam( "nullable", "-711217059")
                .pathParam( "nonEmpty", "-681612133,-681612133")
                .pathParam( "empty", "-707490936")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Items.Size=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NonEmptyItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-203843413")
                .pathParam( "nullable", "-1048839759")
                .pathParam( "nonEmpty", ",-369129623,-472731640")
                .pathParam( "empty", "-671973653")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NullableType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-166256120")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", "-150038121,485495228,998536309")
                .pathParam( "empty", "-791141610")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NullableType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-575065456")
                .pathParam( "nullable", "918")
                .pathParam( "nonEmpty", "-1031405027,510637594,-41580670")
                .pathParam( "empty", "-732336218")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NullableItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-158194533")
                .pathParam( "nullable", "?^")
                .pathParam( "nonEmpty", "-796367007,-11326793,196106126")
                .pathParam( "empty", "-598577556")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_ExplodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "")
                .pathParam( "nullable", "-47825361")
                .pathParam( "nonEmpty", "-360876909,160482921,-905302881")
                .pathParam( "empty", "-123120185")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_ExplodedType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "sdzlzldxdlzzomz=,nzqljukddqgmb=]Y#'")
                .pathParam( "nullable", "-461472705")
                .pathParam( "nonEmpty", "-748673657,551942108,-887294233")
                .pathParam( "empty", "-178649785")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_ExplodedItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "3Cj{(")
                .pathParam( "nullable", "-211455605")
                .pathParam( "nonEmpty", "-714715126,-448547311,989944148")
                .pathParam( "empty", "-223882716")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0,height=0,dypcwmpcqw=n+4e/,prvivamc=X},u (P,,vghxcdimukwskoku=3;Oq9B]B")
                .pathParam( "nullable", "width,0,height,0,ohnhiupk,390.1,djridyouuwadqb,true,jmkakkds,rbchglysq,-870,wgztvsjqeocslw,-898.8,yzi,true")
                .pathParam( "nonEmpty", "width,0,height,0,aqfmjfby,-271.3,jt,o,0T$mT,xrqcwtokeg,W,9@!}Gd,ljq,")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesWidthDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=")
                .pathParam( "nullable", "width,")
                .pathParam( "nonEmpty", "height,")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesWidthType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "height=,imhk=4j#G")
                .pathParam( "nullable", "height,,fmpapzhxplitxb,-788.2,vxpppioawrmcdfj,990,puhqhu,-13")
                .pathParam( "nonEmpty", "width,,phgkubwpvzrqnwnh,true,njioruzhsoollfy,C_,`,sjcjbexpuyrgjiuv,575")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesWidthValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=894242750")
                .pathParam( "nullable", "width,820244111")
                .pathParam( "nonEmpty", "width,568572154,height,227274954")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesHeightValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "height=726524913,yuja=&Pm%` %D,f!jA,:\\,i= \"+#%n,,B5L3@':,avpssyngshwxzaq='FFp3[cD,k")
                .pathParam( "nullable", "height,874585664,tfnyeumxc,")
                .pathParam( "nonEmpty", "tukdvd,uu,Gb,A1,ysklonkvsbveng,V,nyu0(uBY,,qp,true,mjhfagcqmyl,M2i@.lwX,qrtentfmqt,")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "-844.9")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,npabiwv,y:,27HkDZ,rmlisddvyznpjio,h`46,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,-1,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,0,height,1007.7")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,0,height,-1")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "~'tm1Z,>& S{D")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,-392.6")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,-1")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,0,height,lpgycvfdolgu,true,vlidg,true,hztyfr,true")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,0,height,-1")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "251")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=true")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=-1")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0,height=-693.7")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0,height=-1")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_EmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "A")
                .pathParam( "nonEmpty", "S0)")
                .pathParam( "empty", "$")
            .when()
                .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_EmptyValueLength_Is_Gt_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "hhlf~$]%`4'D{wM*8*b<L3%IRa|8C(`KgUnbd:8?1zt\\uM}4-i+U~pW?5^HEi.\\<S)^dq%k\"dTq+>gy-HZt6rZ%Wzx6{<h}W\"Sk}%-Ll/f10( t^]}<M[_JfU29nXt{k]_oNvmA\\*|\\_GMhY&TgKd[m/3[Xi2s[@b'^1)jCHcY=")
                .pathParam( "nonEmpty", "\\gDBQyHj[tB6)2c<2,{r[wp4k9sI;5(@jgGr%5`+=w*aG)@b&1avN")
                .pathParam( "empty", "IsK#EWAwleN\"]Y,9fw7?5 KJYu$s]/HfvwpDFT Dsdo=T.~p4;!D6|${Go|hn\";j^")
            .when()
                .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_EmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "}")
                .pathParam( "nonEmpty", "o%z")
                .pathParam( "empty", "")
            .when()
                .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
            .then()
                // empty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "q")
                .pathParam( "nonEmpty", "")
                .pathParam( "empty", "r")
            .when()
                .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_NonEmptyValueLength_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "!")
                .pathParam( "nonEmpty", "L.")
                .pathParam( "empty", "s")
            .when()
                .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
            .then()
                // nonEmpty.Value.Length=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_NullableType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", "%z&")
                .pathParam( "empty", "b")
            .when()
                .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
            .then()
                // nullable.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), responseHeaders( response));
    }

    private static Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private static Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }

    private static String forTestServer() {
        return forTestServer( null);
    }

    private static String forTestServer( String defaultUri) {
        String testServer = tcasesApiServer();
        return
            defaultUri == null || !testServer.isEmpty()
            ? testServer
            : defaultUri;
    }

    private static String tcasesApiServer() {
        String uri = System.getProperty( "tcasesApiServer");
        return uri == null? "" : uri.trim();
    }

    private static Map<String,List<String>> responseHeaders( Response response) {
        return
            response.getHeaders().asList().stream()
            .collect( groupingBy( Header::getName, mapping( Header::getValue, toList())));
    }
}
