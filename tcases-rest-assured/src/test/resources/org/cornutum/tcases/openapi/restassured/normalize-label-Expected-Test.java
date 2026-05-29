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

public class NormalizeLabelTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ".-140366643.791353862.190316790")
                .pathParam( "empty", "")
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
                .pathParam( "exploded", ".-463395799")
                .pathParam( "nullable", ".-332021383")
                .pathParam( "nonEmpty", "..-288633356.-956637372.981348031.103335176.667290831.721767252.-3085484.-843293232..415309718.3174618")
                .pathParam( "empty", ".-12762885")
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
                .pathParam( "exploded", "..-159852805.131542917.251312859.-171579586.-750539319.-154954766.-948420120.-682940200.-224396516.-761497982.803193639.789269224")
                .pathParam( "nullable", "..244985401.-990469872.-534259002.-739825130.-687157474.1016684757.860560108.266093296.922628584.462793153.-364655689.-858163205")
                .pathParam( "nonEmpty", ".0.-960328635.29464766")
                .pathParam( "empty", "..792360053.291898312.668830193.407642047.531841364.-301054611.-79848832")
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
                .pathParam( "exploded", "")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ".329781726.566086601.-695581581.-1063605459.-150730921.566086601.423335608")
                .pathParam( "empty", ".0")
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
                .pathParam( "exploded", ".0")
                .pathParam( "nullable", ".0")
                .pathParam( "nonEmpty", ".-865537975.-18306239.581042120")
                .pathParam( "empty", ".917681089.917681089.771686334")
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
                .pathParam( "exploded", ".274063893.709322550.-824199225.842466193.-714015532.842466193.551951298.-761010492.-675993972")
                .pathParam( "nullable", ".628758247.696293422.-730288955.-627071149.943322854.84994241.404769634.-169493018.87521984.-977070564.-634602278.60129108.60129108.858185271")
                .pathParam( "nonEmpty", ".0.639961074.-434070739.563009866.817628095.303986754.711825026.-434070739.61353147.192030854")
                .pathParam( "empty", "")
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
                .pathParam( "exploded", "")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ".602299661.-521998344.471704900")
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
                .pathParam( "exploded", "")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ".293285996.-840003085.783495743")
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
                .pathParam( "exploded", "")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ".176727066.267887020.1036901977")
                .pathParam( "empty", ".true")
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
                .pathParam( "exploded", "")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ".507612069.-239649258.-5211739")
                .pathParam( "empty", ".gtzskhvkosmbpaw,941,cxlsy,U~")
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
                .pathParam( "exploded", "")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", "")
                .pathParam( "empty", ".-16015700")
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
                .pathParam( "exploded", "")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", "")
                .pathParam( "empty", ".-704144395")
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
                .pathParam( "exploded", "")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ".DBY2MZ{K")
                .pathParam( "empty", ".-740357632")
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
                .pathParam( "exploded", "")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ".1002102866.1002102866")
                .pathParam( "empty", ".-92083869")
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
                .pathParam( "exploded", "")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ".Zpid~eS.-847436968.416705462")
                .pathParam( "empty", ".-887963699")
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
                .pathParam( "exploded", "")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ".502786065.606726411.656128323")
                .pathParam( "empty", ".-2877783")
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
                .pathParam( "exploded", "")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ".1010217115.125011895.612292472")
                .pathParam( "empty", ".-719848283")
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
                .pathParam( "exploded", "")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ".924610812.-710085587.-507017266")
                .pathParam( "empty", ".-257885851")
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
                .pathParam( "exploded", "")
                .pathParam( "nullable", ".true")
                .pathParam( "nonEmpty", ".404022269.79758989.-331016863")
                .pathParam( "empty", ".-327796475")
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
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ".940261480.7643073.4760441")
                .pathParam( "empty", ".-1071318263")
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
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ".702232520.907520618.-866907843")
                .pathParam( "empty", ".-502394130")
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
                .pathParam( "exploded", ".903.2")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ".716396618.-985652320.269782819")
                .pathParam( "empty", ".-248665960")
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
                .pathParam( "exploded", ".jn,844.1,ptqoaelv,-580.8,egpbzikwvkwj,@T\\Gq7o")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ".1037548820.-109826826.400448556")
                .pathParam( "empty", ".-787745889")
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
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", "")
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
                .pathParam( "exploded", ".width=0.height=0.afwymipzdooi=188.icanh=efs,405.3.wpyuiazic=713.0")
                .pathParam( "nullable", ".width.0.height.0.oakxaueccziz.a ]DkT,")
                .pathParam( "nonEmpty", ".width.0.height.0.dyylsootdbfy.795.8")
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
                .pathParam( "exploded", ".width=")
                .pathParam( "nullable", ".width.")
                .pathParam( "nonEmpty", ".width.")
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
                .pathParam( "exploded", ".height=.hqtxtzckxra=.u=fqfoepdeah,498.3")
                .pathParam( "nullable", ".height..ahrls.true.unc.true")
                .pathParam( "nonEmpty", ".width.543529141.height..z..gxzgei..niezqcyuzpqgdomp.328")
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
                .pathParam( "exploded", ".width=1052730313")
                .pathParam( "nullable", ".width.1071261129")
                .pathParam( "nonEmpty", ".height.1008996130")
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
                .pathParam( "exploded", ".height=505052926.hjfqmrfxoa=true.cuhqamx=rt)(,l`f")
                .pathParam( "nullable", ".height.761714911.imtsswdi.true.nfbrccbhsj.871")
                .pathParam( "nonEmpty", ".width.0.hovnskmtqnuzuev.v]B,VV /n\"")
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
                .pathParam( "exploded", ".width=0")
                .pathParam( "nullable", ".width.0")
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
                .pathParam( "exploded", ".width=0")
                .pathParam( "nullable", ".width.0")
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
                .pathParam( "exploded", ".width=0")
                .pathParam( "nullable", ".width.0")
                .pathParam( "nonEmpty", ".0")
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
                .pathParam( "exploded", ".width=0")
                .pathParam( "nullable", ".width.0")
                .pathParam( "nonEmpty", ".width.,Vt]W`{J,;Z73tB.height.0")
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
                .pathParam( "exploded", ".width=0")
                .pathParam( "nullable", ".width.0")
                .pathParam( "nonEmpty", ".width.-1.height.0")
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
                .pathParam( "exploded", ".width=0")
                .pathParam( "nullable", ".width.0")
                .pathParam( "nonEmpty", ".height.wyjecq,495,zgqiyaxcxnywoz,-843")
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
                .pathParam( "exploded", ".width=0")
                .pathParam( "nullable", ".width.0")
                .pathParam( "nonEmpty", ".height.-1")
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
                .pathParam( "exploded", ".width=0")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ".height.0")
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
                .pathParam( "exploded", ".width=0")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", ".height.0")
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
                .pathParam( "exploded", ".width=0")
                .pathParam( "nullable", ".900")
                .pathParam( "nonEmpty", ".height.0")
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
                .pathParam( "exploded", ".width=0")
                .pathParam( "nullable", ".width.urqbzhothcunn,!=d`m,ox8mx@e$")
                .pathParam( "nonEmpty", ".height.0")
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
                .pathParam( "exploded", ".width=0")
                .pathParam( "nullable", ".width.-1")
                .pathParam( "nonEmpty", ".height.0")
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
                .pathParam( "exploded", ".width=0")
                .pathParam( "nullable", ".width.0.height.true")
                .pathParam( "nonEmpty", ".height.0")
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
                .pathParam( "exploded", ".width=0")
                .pathParam( "nullable", ".width.0.height.-1")
                .pathParam( "nonEmpty", ".height.0")
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
                .pathParam( "nullable", ".width.0")
                .pathParam( "nonEmpty", ".height.0")
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
                .pathParam( "nullable", ".width.0")
                .pathParam( "nonEmpty", ".height.0")
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
                .pathParam( "exploded", ".H~iAJ{,")
                .pathParam( "nullable", ".width.0")
                .pathParam( "nonEmpty", ".height.0")
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
                .pathParam( "exploded", ".width=true")
                .pathParam( "nullable", ".width.0")
                .pathParam( "nonEmpty", ".height.0")
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
                .pathParam( "exploded", ".width=-1")
                .pathParam( "nullable", ".width.0")
                .pathParam( "nonEmpty", ".height.0")
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
                .pathParam( "exploded", ".width=0.height=")
                .pathParam( "nullable", ".width.0")
                .pathParam( "nonEmpty", ".height.0")
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
                .pathParam( "exploded", ".width=0.height=-1")
                .pathParam( "nullable", ".width.0")
                .pathParam( "nonEmpty", ".height.0")
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
                .pathParam( "nullable", ".S0)$yhhlf~$]%`4'D{wM*8*b<L3%IRa|8C(`KgUnbd:8?1zt\\uM}4-i+U~pW?")
                .pathParam( "nonEmpty", ".5^H")
                .pathParam( "empty", ".i.\\<S)^dq%k\"dTq+>gy-HZt6rZ%Wzx6{<h}W\"Sk}%-Ll/f10( t^]}<M[_JfU29nXt{k]_oNvmA\\*|\\_GMhY&TgKd[m/3[Xi2s[@b'^1)jCHcY=x\\gDBQyHj[tB6)2c<2,{r[wp4k9sI;5(@jgGr%5`+=w*aG)@b&1avN2IsK#EWAwleN\"]Y,9fw7?5 KJYu$s]/HfvwpDFT Dsdo=T.~p4;!D6|${Go|hn\";j^)1=$}o%zg\\bD`dep5Ftn")
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
                .pathParam( "nullable", ".")
                .pathParam( "nonEmpty", ".\":SD4:uw,pGY`UWA9GqrV'Au!L.sq*Pf%z&b?lK{2hFEDwG[")
                .pathParam( "empty", ".")
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
                .pathParam( "nullable", ".cSVW%M'|y3{f5xCL8-Cu|k*Ia9A+*A,|L?-G=V]L8CYg t<P]#{k%AlU1l`p?H|Q~4%@~[NgZ> 9M3>cv.$m]f7qH[r3.GC#rU f7R#?y[aaJ#Y:%`$s(A{@\\@s<\"M5rH[vC?>7(^zKpUPb{ CO2qR}\\Dk iF")
                .pathParam( "nonEmpty", ".XrX")
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
                .pathParam( "nullable", ".XH&hN'7dl-FE7F$i?^G*Q\\N x*:/3GOUKk5L\"y^5as@3O-;]90Q2kNWES$tnaIbbEvzR;]Y#'(0I4{Lr>ddf523Cj{(#ur5@!]i@lNx2T>[ T.n+4e/QXQFoMTvrLW#X}bu (P,&tLxMMRrgg;PU{$X/&13;Oq9B]BAEm+ET;&8pY>.SkrSOH<zOh.ISql!.*/Y@S}4zRlE7O]^Oj;R(hB:\\#EOdZX@_#9h>Sx*]@2")
                .pathParam( "nonEmpty", ".vCg")
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
                .pathParam( "nullable", ".xQ`YeX<nq9zrZ\\>W/i'FFp3[cDuks&|t5vCKQjPAF_@KB&HOD7y]8s#C'wGb{A1k?w(RX{T1Jrw#^pfc'V[nyu0(uBY\"upE!j`fgJ${JXIpa67M2i@.lwXgIZjb%WWDG:>2AvL#|L1:8E;u#>Dmy:o27HkDZ%rt)listU>?]sAviyah")
                .pathParam( "nonEmpty", "")
                .pathParam( "empty", ".46\"yhpb\"~'")
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
                .pathParam( "nullable", ".m1ZV>& S{DOg6(3Wtz>n1bF%j|UIqx[@`~t0Yub'x>\"&w<b|xQ#,:t\"S`9T\\[450nSg],I=BTH")
                .pathParam( "nonEmpty", "")
                .pathParam( "empty", ".6;vgsi}{1YsSiTi|>S:c`%\";D'[b&IY0S#=[fBQR|Yk_o#)?iw1sv09rscxxp8%X/#>V|=KN(ip@[:+&rEEMcN.PyrD\\.9M5:</v2\\t|3I;&@@K~$JU@ihst_E[pP)^oQ)x4U1!` `FGJ+1Z~3\"P(Ys|!DI,F) >*$t{egzYY<JB](%E4N9BRTWHzX(0o|HhJW4M.w(:\"QB6?Nu^7M%pKSwr/BFl")
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
                .pathParam( "nullable", ".U\\l,8dK4Ush*wg5cupL@=wy5R:7{*B}O$/dmfFfe^>quwQw]Cb_@R\"-+\"mX(o!+EUV<<>`=CBg2d\"Ow:e6KT6;IUFdOY}vLS&K8^S:F+,*V$`$p.VlD}[!xuGEGh_MV$i:26U'~lb<#Ctwr~#\\d8l= SP^A6>|V@<p idGE`zGK-[#'o/K*#@mPxlw4ejJJBF<m?msiX,SRO\\FRR`~6E'JU(8+l\"o! G-M3VX_UxHz~rHo&3mshX&U{Bu9J_G")
                .pathParam( "nonEmpty", ".I'")
                .pathParam( "empty", ".kwfY})Fo{JuFa}]]Sd8$&x9_%Bh,Bu3p}9%{Q,2e>=R/-)D`0Q;@f_~3+2Dys=ohR+{T8*hW$:%2Z7Udz)VMKq)8$Zc\\DOqfvFSDLh@^8BwBC)q*6(H](E4iEMT205M.w2pMI;$BwM\"4-/cp^f1W<R,',~~6E'{_>(ZTh_wt'k_.uo}b#<`j^P@xz&}?\"e,")
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
                .pathParam( "nonEmpty", ".XS$")
                .pathParam( "empty", ".lN ~[0E(:bVT}nN+D8~L7Ni'\\|_}g7N@:04p}o)\"&0 V|_3bYGA\"vho1:\"%13?BvplkOt4ublrE>JSCa+m&{#pK5{JxIb?+TH#eN7PJo\"Y[b-YBNtB~L{[#pc _qpTW]`v^=H{@2TJiVK2,f#x9fs\\z0U^n;3[,h7 -5)L7~N[?fj\"|_n]elp:aA?k5O}")
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
                .pathParam( "nonEmpty", ".NCL")
                .pathParam( "empty", ".1\"7\"4n'#CeJ@`EJ+{HM/mwM`,.BR0A#!Z3Y!}Zg4DbmFZ;sjRz4\" X}x3Bpak+~?D:?5j_D&;leD)#wGUu?pED[.7c/\\B~PaT\\:4x7j+(}Tn[?V50smO.%s8!QXwg~b*]ALUF&CH~0V_Pd`-3xbZmws?v2Rpj/f1^i^2iVySVmj8~t++\".1L9Mk_d~2x&q}7Lf19C-A Kg~K4XfU^7i&{<p94(=j4*lDd@=q| o?%]s rPt/*vom*\\V:v/&5")
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
