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

public class NormalizeMatrixTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", ";exploded")
                .pathParam( "nullable", ";nullable")
                .pathParam( "nonEmpty", ";nonEmpty=-667734390,52888943,-613703947")
                .pathParam( "empty", ";empty")
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
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyItemsSize_Is_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", ";exploded=-502866499")
                .pathParam( "nullable", ";nullable=-178922770")
                .pathParam( "nonEmpty", ";nonEmpty=,579673678,-123379548,-310726322,408916674,-123379548,833605195,-470221954")
                .pathParam( "empty", ";empty=-146739138")
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
                .pathParam( "exploded", ";exploded;exploded=324498561;exploded=-447569366;exploded=-246773701;exploded=391059911;exploded=1051980808;exploded=-587451820;exploded=-606844601;exploded=-423775159;exploded=-847400667;exploded=-1052151849;exploded=-986748104;exploded=1041524851;exploded=536130786")
                .pathParam( "nullable", ";nullable=,1022475891,1071792615,1003304817,-371443331,-118750903,-743637375,508438038,617143212,-190189331,288416967,-834544207,184418481")
                .pathParam( "nonEmpty", ";nonEmpty=0,808988688,-372617346")
                .pathParam( "empty", ";empty=,-585675863,72596763,-296017831,213256445,1021710089,510153224,229600205,139820056,-902694470,-243357730")
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
                .pathParam( "exploded", ";exploded")
                .pathParam( "nullable", ";nullable")
                .pathParam( "nonEmpty", ";nonEmpty=647363701,463221855,647363701,-1020983073")
                .pathParam( "empty", ";empty=0")
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
                .pathParam( "exploded", ";exploded=0")
                .pathParam( "nullable", ";nullable=0")
                .pathParam( "nonEmpty", ";nonEmpty=-582110256,253195087,192270468")
                .pathParam( "empty", ";empty=875761980,-531738245,486958294,343346691,64986756,-737295796,-147385826,431083565,64986756,-357597705")
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
    public void getPathArrayEmptyNonEmptyNullableExploded_NullableItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", ";exploded=268536239;exploded=621480451;exploded=1047340513;exploded=-789247164;exploded=-287886212;exploded=646483382;exploded=-104528854;exploded=994637034;exploded=496226084;exploded=423537660;exploded=24282505;exploded=691777861;exploded=691777861;exploded=-224601062;exploded=-43482798;exploded=762468838")
                .pathParam( "nullable", ";nullable=331672103,939308768,-893662531,-874186250,-213851462,-772744928,-690599939,-661964290,-468691461,-228899221,-690599939,-69920141,-57884003")
                .pathParam( "nonEmpty", ";nonEmpty=0,131718532,-338157609,-368116076,-338157609,512548671,-871130631,395623599")
                .pathParam( "empty", ";empty")
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
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", ";exploded")
                .pathParam( "nullable", ";nullable")
                .pathParam( "nonEmpty", ";nonEmpty=369756713,1012174457,-974094503")
                .pathParam( "empty", "")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // empty.Defined=No
                .statusCode( isBadRequest())
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
                .pathParam( "exploded", ";exploded")
                .pathParam( "nullable", ";nullable")
                .pathParam( "nonEmpty", ";nonEmpty=573105675,844025164,312440747")
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
                .pathParam( "exploded", ";exploded")
                .pathParam( "nullable", ";nullable")
                .pathParam( "nonEmpty", ";nonEmpty=89483985,-96976226,-446897229")
                .pathParam( "empty", ";empty=0A#!Z3")
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
                .pathParam( "exploded", ";exploded")
                .pathParam( "nullable", ";nullable")
                .pathParam( "nonEmpty", ";nonEmpty=456220268,-677702117,-630282775")
                .pathParam( "empty", ";empty=phnqrg,-884.8,lzlgihskrmr,true")
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
    public void getPathArrayEmptyNonEmptyNullableExploded_NonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", ";exploded")
                .pathParam( "nullable", ";nullable")
                .pathParam( "nonEmpty", "")
                .pathParam( "empty", ";empty=-431535649")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Defined=No
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
                .pathParam( "exploded", ";exploded")
                .pathParam( "nullable", ";nullable")
                .pathParam( "nonEmpty", "")
                .pathParam( "empty", ";empty=-3026385")
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
                .pathParam( "exploded", ";exploded")
                .pathParam( "nullable", ";nullable")
                .pathParam( "nonEmpty", ";nonEmpty=794")
                .pathParam( "empty", ";empty=-176837910")
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
                .pathParam( "exploded", ";exploded")
                .pathParam( "nullable", ";nullable")
                .pathParam( "nonEmpty", ";nonEmpty=634757152,634757152")
                .pathParam( "empty", ";empty=-853497690")
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
                .pathParam( "exploded", ";exploded")
                .pathParam( "nullable", ";nullable")
                .pathParam( "nonEmpty", ";nonEmpty=,-29284074,-946180665")
                .pathParam( "empty", ";empty=-335173784")
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
    public void getPathArrayEmptyNonEmptyNullableExploded_NullableDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", ";exploded")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ";nonEmpty=609732032,342427315,-968734343")
                .pathParam( "empty", ";empty=-32130110")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Defined=No
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
                .pathParam( "exploded", ";exploded")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ";nonEmpty=364242478,222537679,-987010397")
                .pathParam( "empty", ";empty=-577610107")
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
                .pathParam( "exploded", ";exploded")
                .pathParam( "nullable", ";nullable=c")
                .pathParam( "nonEmpty", ";nonEmpty=818914408,518439902,834539361")
                .pathParam( "empty", ";empty=-81576929")
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
                .pathParam( "exploded", ";exploded")
                .pathParam( "nullable", ";nullable=duzedqjf,651.2")
                .pathParam( "nonEmpty", ";nonEmpty=231524417,290226503,667561301")
                .pathParam( "empty", ";empty=-217836452")
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
    public void getPathArrayEmptyNonEmptyNullableExploded_ExplodedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "")
                .pathParam( "nullable", ";nullable")
                .pathParam( "nonEmpty", ";nonEmpty=942935294,-783262995,286177554")
                .pathParam( "empty", ";empty=-336116414")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Defined=No
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
                .pathParam( "nullable", ";nullable")
                .pathParam( "nonEmpty", ";nonEmpty=838625815,277302202,628047030")
                .pathParam( "empty", ";empty=-238641164")
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
                .pathParam( "exploded", ";exploded=-884")
                .pathParam( "nullable", ";nullable")
                .pathParam( "nonEmpty", ";nonEmpty=343897587,757984424,-347740067")
                .pathParam( "empty", ";empty=-603146024")
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
                .pathParam( "exploded", ";exploded=true")
                .pathParam( "nullable", ";nullable")
                .pathParam( "nonEmpty", ";nonEmpty=629688416,-481185912,-110569375")
                .pathParam( "empty", ";empty=-291851879")
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
                .pathParam( "exploded", "")
                .pathParam( "nullable", ";nullable")
                .pathParam( "nonEmpty", ";nonEmpty")
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
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertyCount_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", ";width=0;height=0;hm=718.2")
                .pathParam( "nullable", ";nullable=width,0,height,0,ahuuzljs,821.0")
                .pathParam( "nonEmpty", ";nonEmpty=width,0,height,0,yxubbptfpytcho,mj8~t++\"")
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
                .pathParam( "exploded", ";width")
                .pathParam( "nullable", ";nullable=width,")
                .pathParam( "nonEmpty", ";nonEmpty=width,")
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
                .pathParam( "exploded", ";height;sudvyjsx=640.8;uru=-420.9;pyoxebkxbcogv=998")
                .pathParam( "nullable", ";nullable=height,,rzwb,-597,bipkjfxbd,]s rPt/*")
                .pathParam( "nonEmpty", ";nonEmpty=width,852022644,height,,rjncljsrfgnsz,yyjloqdswuoqm,-819,fj,")
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
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesHeightValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", ";width=339840912")
                .pathParam( "nullable", ";nullable=width,4449220")
                .pathParam( "nonEmpty", ";nonEmpty=height,846785478")
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
                .pathParam( "exploded", ";height=47833503;xckpo=true;wcznoamolspy=true;sbkmwthgjsio=-1003")
                .pathParam( "nullable", ";nullable=height,68160380,eimgdokpxsjxige,/c,PS{u{j>X,kvljgvoab,%")
                .pathParam( "nonEmpty", ";nonEmpty=width,0,uvou,bspwdlfzd,762.6,znxnypvm,668.6,capwftptbcwghyhz,#U_+$f,lzcginjmhkjzjdd,150.8,rqkqpotnrfnmbvwj,495.5")
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
    public void getPathObjectNonEmptyNullableExploded_NonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", ";width=0")
                .pathParam( "nullable", ";nullable=width,0")
                .pathParam( "nonEmpty", "")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Defined=No
                .statusCode( isBadRequest())
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
                .pathParam( "exploded", ";width=0")
                .pathParam( "nullable", ";nullable=width,0")
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
                .pathParam( "exploded", ";width=0")
                .pathParam( "nullable", ";nullable=width,0")
                .pathParam( "nonEmpty", ";nonEmpty=@DVZ#y?,>?H^YNRW,,V")
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
                .pathParam( "exploded", ";width=0")
                .pathParam( "nullable", ";nullable=width,0")
                .pathParam( "nonEmpty", ";nonEmpty=width,593.5,height,0")
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
                .pathParam( "exploded", ";width=0")
                .pathParam( "nullable", ";nullable=width,0")
                .pathParam( "nonEmpty", ";nonEmpty=width,-1,height,0")
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
                .pathParam( "exploded", ";width=0")
                .pathParam( "nullable", ";nullable=width,0")
                .pathParam( "nonEmpty", ";nonEmpty=height,645.4")
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
                .pathParam( "exploded", ";width=0")
                .pathParam( "nullable", ";nullable=width,0")
                .pathParam( "nonEmpty", ";nonEmpty=height,-1")
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
    public void getPathObjectNonEmptyNullableExploded_NullableDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", ";width=0")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ";nonEmpty=height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Defined=No
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
                .pathParam( "exploded", ";width=0")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ";nonEmpty=height,0")
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
                .pathParam( "exploded", ";width=0")
                .pathParam( "nullable", ";nullable=vIQT;")
                .pathParam( "nonEmpty", ";nonEmpty=height,0")
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
                .pathParam( "exploded", ";width=0")
                .pathParam( "nullable", ";nullable=width,520.4")
                .pathParam( "nonEmpty", ";nonEmpty=height,0")
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
                .pathParam( "exploded", ";width=0")
                .pathParam( "nullable", ";nullable=width,-1")
                .pathParam( "nonEmpty", ";nonEmpty=height,0")
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
                .pathParam( "exploded", ";width=0")
                .pathParam( "nullable", ";nullable=width,0,height,")
                .pathParam( "nonEmpty", ";nonEmpty=height,0")
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
                .pathParam( "exploded", ";width=0")
                .pathParam( "nullable", ";nullable=width,0,height,-1")
                .pathParam( "nonEmpty", ";nonEmpty=height,0")
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
    public void getPathObjectNonEmptyNullableExploded_ExplodedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "")
                .pathParam( "nullable", ";nullable=width,0")
                .pathParam( "nonEmpty", ";nonEmpty=height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Defined=No
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
                .pathParam( "nullable", ";nullable=width,0")
                .pathParam( "nonEmpty", ";nonEmpty=height,0")
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
                .pathParam( "exploded", ";exploded=true")
                .pathParam( "nullable", ";nullable=width,0")
                .pathParam( "nonEmpty", ";nonEmpty=height,0")
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
                .pathParam( "exploded", ";width=true")
                .pathParam( "nullable", ";nullable=width,0")
                .pathParam( "nonEmpty", ";nonEmpty=height,0")
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
                .pathParam( "exploded", ";width=-1")
                .pathParam( "nullable", ";nullable=width,0")
                .pathParam( "nonEmpty", ";nonEmpty=height,0")
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
                .pathParam( "exploded", ";width=0;height")
                .pathParam( "nullable", ";nullable=width,0")
                .pathParam( "nonEmpty", ";nonEmpty=height,0")
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
                .pathParam( "exploded", ";width=0;height=-1")
                .pathParam( "nullable", ";nullable=width,0")
                .pathParam( "nonEmpty", ";nonEmpty=height,0")
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
                .pathParam( "nullable", ";nullable=S0)$yhhlf~$]%`4'D{wM*8*b<L3%IRa|8C(`KgUnbd:8?1zt\\uM}4-i+U~pW?5^HEi.\\<S)^dq%k\"dTq+>gy-HZt6rZ%Wz")
                .pathParam( "nonEmpty", ";nonEmpty=x6{")
                .pathParam( "empty", ";empty=h}W\"Sk}%-Ll/f10( t^]}<M[_JfU29nXt{k]_oNvmA\\*|\\_GMhY&TgKd[m/3[Xi2s[@b'^1)jCHcY=x\\gDBQyHj[tB6)2c<2,{r[wp4k9sI;5(@jgGr%5`+=w*a")
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
    public void getPathStringEmptyNonEmptyNullable_EmptyValueLength_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", ";nullable")
                .pathParam( "nonEmpty", ";nonEmpty=)@b&1avN2IsK#EWAwleN\"]Y,9fw7?5 KJYu$s]/HfvwpDFT Dsdo=T.~p4;!D6|${Go|hn\";j^)1=$}o%zg")
                .pathParam( "empty", ";empty")
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
    public void getPathStringEmptyNonEmptyNullable_EmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", ";nullable=bD`dep")
                .pathParam( "nonEmpty", ";nonEmpty=5Ft")
                .pathParam( "empty", "")
            .when()
                .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
            .then()
                // empty.Defined=No
                .statusCode( isBadRequest())
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
                .pathParam( "nullable", ";nullable=A\":SD4:uw,pGY`UWA9GqrV'Au!L.sq*Pf%z&b?lK{2hFEDwG[AcSVW%M'|y3{f5xCL8-Cu|k*Ia9A+*A,|L?-G=V]L8CYg t<P]#{k%AlU1l`p?H|Q~4%@~[Ng")
                .pathParam( "nonEmpty", ";nonEmpty=Z> ")
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
    public void getPathStringEmptyNonEmptyNullable_NonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", ";nullable=(0I4{Lr>ddf523Cj{(#ur5@!]i@lNx2T>[ T.n+4e/QXQFoMTvrLW#X}bu (P,&tLxMMRrgg;PU{$X/&13;Oq9B]BAEm+ET;&8pY>.")
                .pathParam( "nonEmpty", "")
                .pathParam( "empty", ";empty=krSOH<zOh.ISql!.*/Y@S}4zRlE7O]^Oj;R(hB:\\#EOdZX@_#9h>Sx*]@2vCg\\z\"N`DZ<):0'yx-G\\$L MJd0T$mTU|Te.j./Z~wa[oW79@!}GdOxSX4_HQoZsW F4j#G(9$4Z9gxd+p>N0dO\\9hw3dRq9;79pOa~ZqI^em4Hd@2*2WW}8ow RJ=\"8Uu^|B}T\\<*e}$Yl!&( 3hQ.G3P{C_2`OKs914cJYO&=(")
            .when()
                .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
            .then()
                // nonEmpty.Defined=No
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
                .pathParam( "nullable", ";nullable=>{hfh'##I/*uigZ6GmM&Pm%` %DKf!jA,:\\(DkCa \"+#%nm,B5L3@': xQ`YeX<nq9zrZ\\>W/i'FFp3[cDuks&|t5vCKQjPAF_@KB&HOD7y]8s#C'wGb{A1k?w(RX{T1Jrw#^pfc'V[nyu0(uBY\"upE!j`fgJ${JXIpa67M2i@.lwXgIZjb%WWDG:>2AvL#|L1:8E;u#>Dmy:o27HkDZ%")
                .pathParam( "nonEmpty", "")
                .pathParam( "empty", ";empty=t)listU>?]sAviyah`46\"yhpb\"~'tm1ZV>& S{DOg6(3Wtz>n1bF%j|UIqx[@`~t0Yub'x>\"&w<b|xQ#,:t\"S`9T\\[450nSg],I=BTHC6;vgsi}{1YsSiTi|>S:c`%\";D'[b&IY0S#=[fBQR|Yk_o#)?iw1sv09r")
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
                .pathParam( "nullable", ";nullable=i~Y+F>-X(mg(f3NDGZW$sU\\l,8dK4Ush*wg5cupL@=wy5R:7{*B}O$/dmfFfe^>quwQw]Cb_@R\"-+\"mX(o!+EUV<<>`=CBg2d\"Ow:e6KT6;IUFdOY}vLS&K8^S:F+,*V$`$p.VlD}[!xuGEGh_MV$i:26U'~lb<#Ctwr~#\\d8l= SP^A6>|V@<p idGE`zGK-[#'o/K*#@mPxlw4ejJJBF<m?msiX,S")
                .pathParam( "nonEmpty", ";nonEmpty=RO")
                .pathParam( "empty", ";empty=FRR`~6E'JU(8+l\"o! G-M3VX_UxHz~rHo&3mshX&U{Bu9J_GI'ekwfY})Fo{JuFa}]]Sd8$&x9_")
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
    public void getPathStringEmptyNonEmptyNullable_NullableDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ";nonEmpty=%Bh")
                .pathParam( "empty", ";empty=Bu3p}9%{Q,2e>=R/-)D`0Q;@f_~3+2Dys=ohR+{T8*hW$:%2Z7Udz)VMKq)8$Zc\\DOqfvFSDLh@^8B")
            .when()
                .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
            .then()
                // nullable.Defined=No
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
                .pathParam( "nonEmpty", ";nonEmpty=wBC")
                .pathParam( "empty", ";empty=q*6(H](E4iEMT205M.w2pMI;$BwM\"4-/cp^f1W<R,',~~6E'{_>(ZTh_wt'k_.uo}b#<`j^P@xz")
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
