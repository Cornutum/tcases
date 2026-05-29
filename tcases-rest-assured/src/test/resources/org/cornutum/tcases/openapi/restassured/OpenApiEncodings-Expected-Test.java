package org.cornutum.tcases.openapi.restassured;


import org.junit.Test;

import java.util.List;
import java.util.Map;
import static java.util.stream.Collectors.*;

import io.restassured.http.Header;
import io.restassured.response.Response;

import org.cornutum.tcases.openapi.test.ResponseValidator;

import io.restassured.builder.MultiPartSpecBuilder;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class OpenApiEncodingsTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void postMultipart_contentTypes_BodyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertyCount_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "-4324771941249161621.8")
                    .controlName( "elevation")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=-4218000775053017018.1&1=-4065687483600667739.6")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                              82, 107,  53,  87, 103,  73,  87,  98, 113,  98, 108,  89,  81, 112, 109, 103,
                              69,  66,  72,  77, 119,  65,  61,  61
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesElevationValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "0")
                    .controlName( "elevation")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesElevationValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "379075432995927015.5")
                    .controlName( "elevation")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "code=1&city=&tags=LnA&tags=F-m&tags=*R%3A")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=0")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                              47,  83, 120,  56,  90, 118,  90,  72,  80, 121,  71, 121, 109, 117,  54,  52,
                             119,  68,  65,  78, 112,  81,  61,  61
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesLatlongItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "0=442194366911117975.0&1=-4107399077826992051.1")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityValueLength_Is_16() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "-3876459411594404281.8")
                    .controlName( "elevation")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "city=%3F%60-%26%26e%7B9PQvY%26t**")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                              88,  75,  76,  79,  71,  52,  57, 112, 107,  85, 122, 120,  71, 108, 116, 109,
                             112, 119,  98, 121, 101,  65,  61,  61
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityValueLength_Is_Lt_16() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "code=466626405&city=0m%5EsX%21%2B.El%7D&tags=")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=-4348084255471473229.7&1=-4348084255471473229.7")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsSize_Is_Lt_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "0")
                    .controlName( "elevation")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "tags=64%3C")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                              81,  84,  67,  82,  67,  89,  83, 109, 108, 101,  79,  74, 114,  77,  52, 109,
                             114, 102, 103,  73,  84,  65,  61,  61
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMediaType_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "text/xml")
                .request().body( "{\"ze\":[\")d&#k_;b\",\"J|?`F\"],\"yvnfpl\":\";8HR\",\"ir\":398}")
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.Media-Type=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesElevationType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "null")
                    .controlName( "elevation")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-4093221589402708745.7")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.elevation.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesElevationType_Is_NotNumber() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[]")
                    .controlName( "elevation")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-3855178571089511549.9")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.elevation.Type=Not number
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesLatlongType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesLatlongType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "integer=-879")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesLatlongItemsSize_Is_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "0=0")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Items.Size=1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesLatlongItemsSize_Is_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-4210939664381722987.3&2=-4210939664381722987.3")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Items.Size=3
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesLatlongItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "0&1=-4295788794819581759.8")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesLatlongItemsContainsType_Is_NotNumber() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "0=%3D.m%2CQ%23id&1=-4484296376020366955.2")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Items.Contains.Type=Not number
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-4504749090083615716.0")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "number=798.1")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-4585393220638427708.0")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "code=1&city")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-3936847141488164375.6")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.city.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "code=1&city=-31.9")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-4085004214711016567.5")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.city.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "code=1&city=hqg%3BlUXtdid%25%7C%3BJ9%5B")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-4155171155949056928.2")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.city.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCodeType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "code&city=")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-3842840254162967623.6")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.code.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCodeType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "code=o&code=1D%7E2%40&city=")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-4243873936735959279.1")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.code.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCodeValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "code=0&city=")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-3834325275935558032.4")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.code.Value.Is=0
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "code=1&city=&tags")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-3772942961805824404.1")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "code=1&city=&tags=619.5")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-4529902434706142052.3")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsSize_Is_4() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "code=1&city=&tags=lO3&tags=lO3&tags=%7C%28%26&tags=Y%3F%5C")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-3766976501376953009.4")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Size=4
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "code=1&city=&tags&tags=%3F%5Bw&tags=%3FXS")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-4167542713804088477.2")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "code=1&city=&tags=vincecnkmkeqcxmt%2Ctrue%2Cjqgwuzzd%2C343.3&tags=Z*z&tags=mb%5D")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-4535557278930728443.3")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "code=1&city=&tags=F%3E&tags=%5B%7Df&tags=rqO")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-4327711651183678897.4")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_4() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "code=1&city=&tags=U%7EBc&tags=%3F2c&tags=XDX")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-4559539283013593225.2")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=4
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAddressValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "code=1&city=&neaqkzhuutohy=true&ozaqtjpwzxtc=u&uxkgqboqexg=true&anejr=-967")
                    .controlName( "address")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-4399446910261121567.3")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesMapDigestType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-4224722864725326836.1")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "")
                    .controlName( "map-digest")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.map-digest.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesMapDigestType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-3722509201187492557.2")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "352.3")
                    .controlName( "map-digest")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.map-digest.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesMapDigestValueLength_Is_15() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-3861029513285844687.0")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                             118,  77,  76, 109,  52,  55, 102,  79,  83,  83,  51,  73,  77,  53,  50,  66,
                              70, 108,  79,  74
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.map-digest.Value.Length=15
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesMapDigestValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-3733746255302717658.6")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                              83,  89,  90,  77,  48, 105,  72, 120, 100,  99,  87,  99,  49,  53,  65,  99,
                              90,  50,  65,  89,  55,  77,  77,  61
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.map-digest.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_contentTypes_BodyMultipartFormDataValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "0=0&1=-4608523086177055166.6")
                    .controlName( "lat/long")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "-561")
                    .controlName( "kshuuzspquiko")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_contentTypes")
            .then()
                // Body.multipart-form-data.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_contentTypes", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_contentTypes", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
            .when()
                .request( "POST", "/multipart_default")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertyCount_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "-4078904636850734827.6")
                    .controlName( "elevation")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "{}")
                    .controlName( "address")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[-4079848570205714995.5,-3761195149199169746.2]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                              83, 118,  52, 119, 113,  69, 116,  86, 122,  75, 110, 117, 118,  71,  66, 103,
                             104,  51, 100, 110,  72,  81,  61,  61
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesElevationValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "0")
                    .controlName( "elevation")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesElevationValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "251899329347721085.7")
                    .controlName( "elevation")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"tags\":[\"<O7\",\"j8.\",\"|\\\\w\"]}")
                    .controlName( "address")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,0]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                              66,  70,  48,  98, 113, 103, 108,  68,  56,  43,  81,  51, 122,  69, 110,  86,
                              82,  56,  49,  89,  76, 103,  61,  61
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesLatlongItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[813538470860838050.0,-4197772668412705807.0]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityValueLength_Is_16() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "-4352685122587728248.5")
                    .controlName( "elevation")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"v7Zk~=u9j_w@=q}U\"}")
                    .controlName( "address")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                              47, 110,  66, 122, 119,  76,  73,  86,  55, 111,  56, 113,  75,  86,  76,  84,
                              82, 110,  49,  50, 107, 103,  61,  61
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityValueLength_Is_Lt_16() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":496666664,\"city\":\"!(R!'.\",\"tags\":[]}")
                    .controlName( "address")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[-4575535177375062103.8,-4575535177375062103.8]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsSize_Is_Lt_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "0")
                    .controlName( "elevation")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "{\"tags\":[\"<3C\",\"<3C\"]}")
                    .controlName( "address")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                              50,  76,  81, 101,  77,  54, 104, 122, 114,  89,  67,  85,  75, 119, 100,  78,
                              73, 119,  84,  88, 103,  81,  61,  61
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMediaType_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "text/xml")
                .request().body( "[\"\",\":B' \\\\Y@\"]")
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.Media-Type=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesElevationType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "")
                    .controlName( "elevation")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-3693777878646118303.4]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.elevation.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesElevationType_Is_NotNumber() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[\"x&h8'\"]")
                    .controlName( "elevation")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-4338566157590159990.1]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.elevation.Type=Not number
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesLatlongType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesLatlongType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "true")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesLatlongItemsSize_Is_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[0]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Items.Size=1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesLatlongItemsSize_Is_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-4133020463454053345.7,0]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Items.Size=3
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesLatlongItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[null,-4113581948422604106.8]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesLatlongItemsContainsType_Is_NotNumber() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[\"_0+h1B}/\",-4483765363324934100.7]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Items.Contains.Type=Not number
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-4438431708127145517.4]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "true")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-4400533109304594458.1]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":1,\"city\":null}")
                    .controlName( "address")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-4580642992728371947.0]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.city.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":1,\"city\":[\"|L>kQq \"]}")
                    .controlName( "address")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-4276004052025994477.6]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.city.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"p2>m%5}QiBW#br3Gk\"}")
                    .controlName( "address")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-4013917086172889569.6]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.city.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCodeType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":null,\"city\":\"\"}")
                    .controlName( "address")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-3930104463893582644.0]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.code.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCodeType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":[\"9\"],\"city\":\"\"}")
                    .controlName( "address")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-4574227631249896210.7]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.code.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCodeValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":0,\"city\":\"\"}")
                    .controlName( "address")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-4502820839014326931.4]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.code.Value.Is=0
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"tags\":null}")
                    .controlName( "address")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-4065207398236513357.7]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"tags\":800}")
                    .controlName( "address")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-4178922660606841825.6]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsSize_Is_4() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"tags\":[\"88s\",\"={D\",\"#i(\",\"#i(\"]}")
                    .controlName( "address")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-4154198394222776724.6]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Size=4
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"tags\":[null,\"y}Z\",\"q?+\"]}")
                    .controlName( "address")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-3992876299421815024.5]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"tags\":[true,\"&z%\",\"0XA\"]}")
                    .controlName( "address")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-3843241744358783781.9]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"tags\":[\"tQ\",\"65#\",\"QV'\"]}")
                    .controlName( "address")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-4006479944272722696.6]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_4() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"tags\":[\"<eAX\",\"/Pk\",\"@N,\"]}")
                    .controlName( "address")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-3781098012816792440.5]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=4
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAddressValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"xolglerguuiatskl\":305.1,\"z\":{\"uqo\":\"\",\"xvwijutodxaegisf\":[\"_Y+\",\"A^)\"]},\"nqtqntvwc\":[\"@Xz\"]}")
                    .controlName( "address")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-4505905942159561537.2]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesMapDigestType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-4162757515595773646.3]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "")
                    .controlName( "map-digest")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.map-digest.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesMapDigestType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-4191264541299383479.9]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "805")
                    .controlName( "map-digest")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.map-digest.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesMapDigestValueLength_Is_15() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-3937338296360678943.9]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                              88, 113, 119,  86,  78, 111, 118,  55, 110,  50,  98, 118, 104,  75,  79, 102,
                              80,  86,  75,  82
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.map-digest.Value.Length=15
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesMapDigestValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-4479069652227942639.3]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                              86, 110,  90,  53, 114,  71, 104, 101,  74,  52,  43, 103,  49,  71,  98, 115,
                              73,  54, 117,  51, 118, 110,  52,  61
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.map-digest.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_default_BodyMultipartFormDataValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-4254444248109853538.9]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "")
                    .controlName( "gr")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "true")
                    .controlName( "jlqnb")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_default")
            .then()
                // Body.multipart-form-data.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertyCount_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "number=-3738286561866495815.7")
                    .controlName( "elevation")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "{}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "WKLVFFILCWLRCAAQ")
                    .header( "X-Address-Code", "3920351497083377")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[-4107477500180734963.2,-4050968780633960995.2]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .header( "X-Lat-Long", "")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                              83,  85, 104, 119, 104,  74,  53, 102, 113,  73, 109,  75,  89,  54,  57, 115,
                             104, 115,  86,  43, 116,  81,  61,  61
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesElevationValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "number=0")
                    .controlName( "elevation")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesElevationValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "number=873900087119562425.2")
                    .controlName( "elevation")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":1,\"city\":\"\",\"tags\":[\"[R \",\".`:\",\"Vk&\"]}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[0,0]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                              87,  70,  76,  65,  48,  51, 120,  66, 118, 114,  74, 109,  47, 108,  71, 113,
                              88,  57,  87, 105,  69, 103,  61,  61
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[533575267419501795.0,-4363403864537596077.9]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .header( "X-Lat-Long", "-4199585651685514346.3")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongHeadersXLatLongItemsSize_Is_Gt_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "number=-4092782376428212275.7")
                    .controlName( "elevation")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"uc7r)]Mecu9ZVb<+\"}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "HBEHOVLWMNDKAJAH")
                    .header( "X-Address-Code", "8690861387302232")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[-4300089589540551355.0,-4300089589540551355.0]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .header( "X-Lat-Long", "0,-4378643264998345010.7,-4070321916147457067.0,-3943105320469403961.1,-3909728320823778530.4,-4391691642410958997.6,-4557402658915102804.4,-3827718294158536120.1,-4529138620027324304.5,-4404458138294031565.8,-4439827205847798969.3,-4057076439885925211.8")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                              79, 117, 105,  66, 109,  52,  67,  73, 106,  78,  72,  83,  43,  67,  80,  72,
                             104,  67, 110,  83,  65, 119,  61,  61
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongHeadersXLatLongItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-3985226344607903759.8]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .header( "X-Lat-Long", "54937179194340943.9")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongHeadersXLatLongItemsUnique_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "number=0")
                    .controlName( "elevation")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":1002138411,\"tags\":[]}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[890577655913279558.4,890577655913279558.4]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .header( "X-Lat-Long", "-4250983828780030112.9,-3992578205478392188.6,-3762463714465545890.5,-3886730682137020129.6,-3937792357768703891.3,-4607643689155119830.6,-3892324833074414332.4,-3937792357768703891.3,-3701014693755876925.9")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                             107, 103,  73,  47,  70,  43, 106, 108,  74, 100, 109, 108,  51,  82,  88,  77,
                             117,  54, 111,  67, 101,  81,  61,  61
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityValueLength_Is_Lt_16() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"j/UWAY*SNe|S\"}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "MIJHJZUPHURTIASE")
                    .header( "X-Address-Code", "2862766101818084")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsSize_Is_Lt_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "number=622612705077514148.5")
                    .controlName( "elevation")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":1,\"tags\":[\"k8e\"]}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[-3881613909760010108.1,-4261764116791902674.8]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                             122, 120,  73, 109,  65,  70,  67, 103,  70,  52,  79,  48,  65,  47,  69,  73,
                              85,  43,  53,  72, 118, 119,  61,  61
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMediaType_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "text/plain")
                .request().body( "{\"rnvxlwzicbdsebaq\":848,\"gvgu\":593}")
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.Media-Type=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesElevationType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "")
                    .controlName( "elevation")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.elevation.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesElevationType_Is_NotNumber() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "0=C2f%23%3C&1=&2=Ul")
                    .controlName( "elevation")
                    .mimeType( "application/x-www-form-urlencoded")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.elevation.Type=Not number
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{}")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongItemsSize_Is_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[0]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .header( "X-Lat-Long", "")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Items.Size=1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongItemsSize_Is_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[0,-3777333898910334065.9,-3777333898910334065.9]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .header( "X-Lat-Long", "")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Items.Size=3
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[null,null]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .header( "X-Lat-Long", "")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongItemsContainsType_Is_NotNumber() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[true,true]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .header( "X-Lat-Long", "")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Items.Contains.Type=Not number
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongHeadersXLatLongType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[0,0]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .header( "X-Lat-Long", "")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Headers.X-Lat-Long.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongHeadersXLatLongType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[0,0]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .header( "X-Lat-Long", "5DB=,")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Headers.X-Lat-Long.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongHeadersXLatLongItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[0,0]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .header( "X-Lat-Long", "")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Headers.X-Lat-Long.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesLatlongHeadersXLatLongItemsContainsType_Is_NotNumber() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "[0,0]")
                    .controlName( "lat/long")
                    .mimeType( "text/plain")
                    .header( "X-Lat-Long", "")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.latlong.Headers.X-Lat-Long.Items.Contains.Type=Not number
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "55")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":null}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "RDQWMQRJBEOVCJEG")
                    .header( "X-Address-Code", "0081507524057714")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.city.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":{\"ulangyklfaakp\":-250.7}}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "LRYIAQKKKIVDHQNO")
                    .header( "X-Address-Code", "7150088525075711")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.city.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCityValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"t,=4PW?<Ox[Onh_c<\"}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "WMHVEVQAFMCDZDAD")
                    .header( "X-Address-Code", "8851001909973350")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.city.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCodeType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":null,\"city\":\"\"}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "MWNLSMUBIIKOBAPQ")
                    .header( "X-Address-Code", "0217900675562941")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.code.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCodeType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":\"_\",\"city\":\"\"}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "ZZVCKUGVCUUSHYVG")
                    .header( "X-Address-Code", "5237846370708761")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.code.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesCodeValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"code\":0,\"city\":\"\"}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "WLCKBSRXKFBVMRFH")
                    .header( "X-Address-Code", "1368525256317142")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.code.Value.Is=0
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"\",\"tags\":null}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "BZOTDDVTEFUNXMYF")
                    .header( "X-Address-Code", "7825388645324393")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"\",\"tags\":\"5r#\"}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "DLDSCITONOLGMHVI")
                    .header( "X-Address-Code", "9626287066491670")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsSize_Is_4() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"\",\"tags\":[\"m%n\",\"HVY\",\"D>8\",\"D>8\"]}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "YKMVYBIINVAJVZMG")
                    .header( "X-Address-Code", "3119974200127056")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Size=4
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"\",\"tags\":[null,\"<{2\",\"{7R\"]}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "TXFDECYPTQLAXBKM")
                    .header( "X-Address-Code", "2194556437995322")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"\",\"tags\":[{\"mni\":\"2#\",\"j\":[\"hBHH6A\",\"uQ+\\\\r\",\"8i\"],\"dpwdzklwxjxw\":true},\"FAO\",\":Rg\"]}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "YUVBGLXNXGERGEMY")
                    .header( "X-Address-Code", "1348999121194416")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"\",\"tags\":[\"[N\",\"Ta\\\"\",\"|&C\"]}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "CJVNRPQCKOHBOCXG")
                    .header( "X-Address-Code", "5349126381852880")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_4() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"\",\"tags\":[\"9\\\"}3\",\"kA,\",\"OB%\"]}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "NKBUXVAOJWJHWRUB")
                    .header( "X-Address-Code", "2116633326423109")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=4
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"\",\"azrjaetfnetyee\":506.8,\"qbumzkajdvvdhq\":797.1,\"frxmimbfidx\":-701}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "GKVGREJNSGWLKZXI")
                    .header( "X-Address-Code", "7173860158277274")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressIdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"\"}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "")
                    .header( "X-Address-Code", "0639382951419037")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Id.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressIdType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"\"}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "true")
                    .header( "X-Address-Code", "6786270631889512")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Id.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressIdValueLength_Is_15() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"\"}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "@I/!m9$AeY9I$Z;")
                    .header( "X-Address-Code", "0294374407251651")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Id.Value.Length=15
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressIdValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"\"}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", "jA\\8A#ZQ2o[@M,LJ)")
                    .header( "X-Address-Code", "1120173376534488")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Id.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressIdValueMatchesPattern_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"\"}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Id", " )(9{yA+2]_sP_W9")
                    .header( "X-Address-Code", "1874516699212981")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Id.Value.Matches-Pattern=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressCodeType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"\"}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Code", "")
                    .header( "X-Address-Id", "YIKFVEXXSZMGJZJG")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Code.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressCodeType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"\"}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Code", "FxUb")
                    .header( "X-Address-Id", "PVRFQZRYURLEJEHN")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Code.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressCodeValueLength_Is_15() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"\"}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Code", "t78/*+u_16+CPS$")
                    .header( "X-Address-Id", "TNSONKLAOHYAFFHQ")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Code.Value.Length=15
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressCodeValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"\"}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Code", "6yb!5Q'<;OL2U<s\\&")
                    .header( "X-Address-Id", "NIZHZHJXFPWEDSMJ")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Code.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAddressHeadersXAddressCodeValueMatchesPattern_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "{\"city\":\"\"}")
                    .controlName( "address")
                    .mimeType( "text/plain")
                    .header( "X-Address-Code", "J<No2Toyy{v&5~zq")
                    .header( "X-Address-Id", "VVKRCVZZMXRPFLRS")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.address.Headers.X-Address-Code.Value.Matches-Pattern=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesMapDigestType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "")
                    .controlName( "map-digest")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.map-digest.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesMapDigestType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder( "true")
                    .controlName( "map-digest")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.map-digest.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesMapDigestValueLength_Is_15() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                              69, 116,  83,  73,  49,  98, 119, 118,  67,  79, 118,  66,  49,  85,  81,  84,
                             120,  99, 108, 100
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.map-digest.Value.Length=15
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesMapDigestValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                              65,  71,  82, 106,  51,  52,  50,  83, 114, 117, 120,  78,  90, 109,  88,  72,
                             110,  47, 122, 108,  81,  98,  65,  61
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.map-digest.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postMultipart_headers_BodyMultipartFormDataValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "multipart/form-data")
                .multiPart(
                    new MultiPartSpecBuilder(
                        new byte[] {
                             121,  90, 120,  67,  54,  43,  98,  48,  81,  84,  68,  86,  77, 104,  52, 108,
                              50,  75,  65,  49,  98, 103,  61,  61
                        })
                    .controlName( "map-digest")
                    .mimeType( "application/octet-stream")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "[\"2Y)H*_HI\"]")
                    .controlName( "oorcvymfobfdyjfs")
                    .mimeType( "text/plain")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "{\"e\":[\"&\",\"HK\"]}")
                    .controlName( "kgvcswmzsrjduzwz")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
                .multiPart(
                    new MultiPartSpecBuilder( "{\"dudmla\":\";Vvv)N^\",\"dsiduphj\":true}")
                    .controlName( "xovvhejni")
                    .mimeType( "application/json")
                    .emptyFileName()
                    .build())
            .when()
                .request( "POST", "/multipart_headers")
            .then()
                // Body.multipart-form-data.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/multipart_headers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/multipart_headers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertyCount_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", "-3998686662832137669.0")
                .formParam( "lat/long", "-3831399503999759339.7")
                .formParam( "lat/long", "-3896946869424179604.7")
                .formParam( "map-digest", "/k5l0sSjM+6PV0ybEXVtaA==")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", "0")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", "13861742107148630.7")
                .formParam( "code", "1")
                .formParam( "city", "")
                .formParam( "tags", "/Fz,.bS,!Al")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "0")
                .formParam( "map-digest", "LqamvNvZl/z68ZKFrwbLCQ==")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "417098339751014037.5")
                .formParam( "lat/long", "-4610839891197091495.2")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityValueLength_Is_16() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", "-4596298842289608675.7")
                .formParam( "city", "3:gY'aO2@Kf=v6k\"")
                .formParam( "map-digest", "7cX6sbGyto3edm1Oo642kQ==")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityValueLength_Is_Lt_16() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "code", "89167223")
                .formParam( "city", "z6V\\34Y")
                .formParam( "tags", "")
                .formParam( "lat/long", "-4458102162249783838.8")
                .formParam( "lat/long", "-4458102162249783838.8")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsSize_Is_Lt_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", "0")
                .formParam( "tags", "=pb,=pb")
                .formParam( "map-digest", "6ZmP4xqn+1QS22myTgAXYg==")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyMediaType_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/json")
                .request().body( "true")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.Media-Type=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "0", "k@z)EI]")
                .formParam( "1", "q:A4ME@N")
                .formParam( "2", "r^@-w)0")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", (String) null)
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-3846581628495526360.1")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.elevation.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationType_Is_NotNumber() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", "true")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4205110423490623049.4")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.elevation.Type=Not number
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", (String) null)
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "-508.8")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsSize_Is_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Size=1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsSize_Is_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-3915827211230057322.0")
                .formParam( "lat/long", "0")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Size=3
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", (String) null)
                .formParam( "lat/long", "-4154195997442164593.7")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsContainsType_Is_NotNumber() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "")
                .formParam( "lat/long", "-4409108488460500920.1")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Contains.Type=Not number
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", (String) null)
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4461127477671283778.5")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", "-93")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-3742893752063002545.3")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "code", "1")
                .formParam( "city", (String) null)
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4224984153524482814.2")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.city.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "code", "1")
                .formParam( "city", "995.5")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4342000669662094659.1")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.city.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "code", "1")
                .formParam( "city", "Eg%{&- G6-_Q51lzg")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4348394750164631116.0")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.city.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCodeType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "code", (String) null)
                .formParam( "city", "")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4259785774777045582.5")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.code.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCodeType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "code", "")
                .formParam( "city", "")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4239146502563760825.2")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.code.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCodeValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "code", "0")
                .formParam( "city", "")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-3851411250242951552.3")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.code.Value.Is=0
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "code", "1")
                .formParam( "city", "")
                .formParam( "tags", (String) null)
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4042043636966579916.8")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "code", "1")
                .formParam( "city", "")
                .formParam( "tags", "ynz,-130")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-3980122668371922873.3")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsSize_Is_4() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "code", "1")
                .formParam( "city", "")
                .formParam( "tags", "vlD,#VY,#VY,)`-")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4350213649194803421.9")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Size=4
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "code", "1")
                .formParam( "city", "")
                .formParam( "tags", ",YDY,UA2")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4027586750120666447.5")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "code", "1")
                .formParam( "city", "")
                .formParam( "tags", "-338,G[G,:u.")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4566445686792137918.2")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "code", "1")
                .formParam( "city", "")
                .formParam( "tags", "sd, ib,_Vl")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-3783333875823203766.4")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_4() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "code", "1")
                .formParam( "city", "")
                .formParam( "tags", "3tX~,s1V,m`v")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4364526684768019901.0")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=4
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "code", "1")
                .formParam( "city", "")
                .formParam( "q", "372")
                .formParam( "xya", "970.3")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4073962475009505034.6")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4209845244247233358.4")
                .formParam( "map-digest", (String) null)
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4352639283157696290.2")
                .formParam( "map-digest", "true")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestValueLength_Is_15() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4404719267180879025.8")
                .formParam( "map-digest", "5ZSrbrnwglhrga7UGzZG")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Value.Length=15
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-3964927207598385086.6")
                .formParam( "map-digest", "kSPdOmHXe5Sws6i4/Sy2aWs=")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_default_BodyApplicationXWwwFormUrlencodedValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4462379762071076658.9")
                .formParam( "vyt", "^o0QP")
            .when()
                .request( "POST", "/urlencoded_default")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_default", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_default", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertyCount_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", "-4382346925940575975.4")
                .formParam( "lat/long", "-4607409851292293958.0")
                .formParam( "lat/long", "-3759396663344543466.0")
                .formParam( "map-digest", "5hfl3TwiAPMkI/ZSSkFuww==")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", "0")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", "662609628787642021.3")
                .formParam( "address[code]", "1")
                .formParam( "address[city]", "")
                .formParam( "address[tags]", "[cr,9C$,GOz")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "0")
                .formParam( "map-digest", "y+92eC0YLd8SubIByjcHUQ==")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "587611725918046378.2")
                .formParam( "lat/long", "-4461204904884205097.3")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityValueLength_Is_16() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", "-4281331904914510301.6")
                .formParam( "address[city]", "S-dRt;%bYFk:;)\\S")
                .formParam( "map-digest", "QDdYGE7Egkrkf8cuTgpxDQ==")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityValueLength_Is_Lt_16() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address[code]", "342176840")
                .formParam( "address[city]", "JH9Hy ")
                .formParam( "address[tags]", "")
                .formParam( "lat/long", "-3827850361583103461.2")
                .formParam( "lat/long", "-3827850361583103461.2")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsSize_Is_Lt_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", "0")
                .formParam( "address[tags]", "yh8,yh8")
                .formParam( "map-digest", "0uswUyrG7Ua1R8Ym5G/6RQ==")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyMediaType_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/json")
                .request().body( "[\"-wa\\\\nO\",\"\",\"4t|Ad\"]")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.Media-Type=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "number", "-540.9")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", (String) null)
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4277839138107167965.5")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.elevation.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationType_Is_NotNumber() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", "true")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4119616474700835606.2")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.elevation.Type=Not number
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", (String) null)
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsSize_Is_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Size=1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsSize_Is_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-3774962297131703597.9")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Size=3
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", (String) null)
                .formParam( "lat/long", "-4549235185142484911.0")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsContainsType_Is_NotNumber() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "")
                .formParam( "lat/long", "-4134015964346369306.7")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Contains.Type=Not number
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", (String) null)
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-3900330949223035385.9")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", "-483")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4007648804985541891.3")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address[code]", "1")
                .formParam( "address[city]", (String) null)
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4428275457421708503.2")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.city.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address[code]", "1")
                .formParam( "address[city]", "")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4452094661525267628.1")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.city.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address[code]", "1")
                .formParam( "address[city]", "Mo, h<%7?\\h,a,aAx")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4137144908018510158.8")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.city.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCodeType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address[code]", (String) null)
                .formParam( "address[city]", "")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4033770580560188765.4")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.code.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCodeType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address[code]", "]jAbz")
                .formParam( "address[city]", "")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-3789019386122965306.1")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.code.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCodeValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address[code]", "0")
                .formParam( "address[city]", "")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4402795939224895191.9")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.code.Value.Is=0
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address[code]", "1")
                .formParam( "address[city]", "")
                .formParam( "address[tags]", (String) null)
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-3822062191158745950.9")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address[code]", "1")
                .formParam( "address[city]", "")
                .formParam( "address[tags]", "dkuimssmawza,true,xabwfytysbhad,iR")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-3950022034912146813.3")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsSize_Is_4() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address[code]", "1")
                .formParam( "address[city]", "")
                .formParam( "address[tags]", "us<,+&&,SN/,us<")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4505474129496379176.9")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Size=4
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address[code]", "1")
                .formParam( "address[city]", "")
                .formParam( "address[tags]", ",&Q;,YJ*")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4225183037717771394.8")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address[code]", "1")
                .formParam( "address[city]", "")
                .formParam( "address[tags]", "{,HJ3Bk,1KD,4@4")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4341239672737023386.1")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address[code]", "1")
                .formParam( "address[city]", "")
                .formParam( "address[tags]", "?m,2RX,hT=")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4318241857326435889.7")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_4() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address[code]", "1")
                .formParam( "address[city]", "")
                .formParam( "address[tags]", "_hS[,C*8,UP.")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4364613426026794086.1")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=4
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address[code]", "1")
                .formParam( "address[city]", "")
                .formParam( "address[wshn]", "Y2RYZM")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4489652808583587003.8")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4556798295297018371.4")
                .formParam( "map-digest", (String) null)
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4395453557609969149.3")
                .formParam( "ztbba", "true")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestValueLength_Is_15() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4256938890633588632.1")
                .formParam( "map-digest", "KDBxpYQsqeypHv6a3rFm")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Value.Length=15
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-3757828960253640732.3")
                .formParam( "map-digest", "vZJjoUpdC1ZK4SgOeBcMy6k=")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_exploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0")
                .formParam( "lat/long", "-4522238280263383392.0")
                .formParam( "kkgbjevmljvs", "")
                .formParam( "hxen", ",%P_Lh")
                .formParam( "hkuqrhyqydy", "^m]U<")
            .when()
                .request( "POST", "/urlencoded_exploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_exploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_exploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertyCount_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", "-3988902780959288612.5")
                .formParam( "address", "")
                .formParam( "lat/long", "-3934393210868357343.8 -3734597850335825333.6")
                .formParam( "map-digest", "rQfnnAln9txGk8KbHwWgGQ==")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", "0")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", "394041228229495625.6")
                .formParam( "address", "code,1,city,,tags,J.h,O0E,S3O")
                .formParam( "lat/long", "0 0")
                .formParam( "map-digest", "p0NLB41qsMsPzSzeb0BVtA==")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "376875136045802848.3 -4367658121063363170.8")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityValueLength_Is_16() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", "-4211023357451962690.6")
                .formParam( "address", "city,g!h0EwK:*%8)G3{Q")
                .formParam( "map-digest", "P0s7/0ahr1X13ts50wAD1A==")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityValueLength_Is_Lt_16() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", "code,184413177,city,yJ^@?{s)C2,tags,")
                .formParam( "lat/long", "-4528497945011561983.4 -4528497945011561983.4")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsSize_Is_Lt_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", "0")
                .formParam( "address", "tags,]^{")
                .formParam( "map-digest", "8iZ3fBm0r2JcjD2KMumAmw==")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyMediaType_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "text/xml")
                .request().body( "true")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.Media-Type=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "0", "b")
                .formParam( "1", "h;")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", (String) null)
                .formParam( "lat/long", "0 -4472747738610876664.2")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.elevation.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesElevationType_Is_NotNumber() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "elevation", "nrfkhcplaiuqv,493,vbtmrcjfyydn,true,tlxpnvr,-702.2")
                .formParam( "lat/long", "0 -4433701885234587986.3")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.elevation.Type=Not number
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", (String) null)
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsSize_Is_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Size=1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsSize_Is_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0 0 -4524380403214040577.6")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Size=3
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", " -3934995235703426450.7")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesLatlongItemsContainsType_Is_NotNumber() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "true -3957608833212774747.0")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.latlong.Items.Contains.Type=Not number
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", (String) null)
                .formParam( "lat/long", "0 -3991748637860827224.8")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", "-213")
                .formParam( "lat/long", "0 -4043289586316199449.5")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", "code,1,city,")
                .formParam( "lat/long", "0 -4018353002142844834.7")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.city.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", "code,1,city,OJ8%")
                .formParam( "lat/long", "0 -3842116991819939652.0")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.city.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCityValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", "code,1,city,pQ}WMTWD:oH}jv2k3")
                .formParam( "lat/long", "0 -4209336875775017536.0")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.city.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCodeType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", "code,,city,")
                .formParam( "lat/long", "0 -4441556724506639842.8")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.code.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCodeType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", "code,xgivbzr,true,city,")
                .formParam( "lat/long", "0 -3729244121791154116.4")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.code.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesCodeValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", "code,0,city,")
                .formParam( "lat/long", "0 -3843510223049509112.2")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.code.Value.Is=0
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", "code,1,city,,tags,")
                .formParam( "lat/long", "0 -4388530357581672993.8")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", "code,1,city,,tags,-1004.0")
                .formParam( "lat/long", "0 -3821973580652302560.3")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsSize_Is_4() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", "code,1,city,,tags,g'a,*~i,w]/,*~i")
                .formParam( "lat/long", "0 -3724132194911234254.0")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Size=4
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", "code,1,city,,tags,,mrQ,hO\"")
                .formParam( "lat/long", "0 -3778053685970423944.4")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", "code,1,city,,tags,kwztal,-1015,hykxdi,q^1,0xNt>$&<,,U(',*qC")
                .formParam( "lat/long", "0 -4040307637531959817.0")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", "code,1,city,,tags, D,!H2,OqC")
                .formParam( "lat/long", "0 -3829286745098239095.8")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesTagsItemsContainsValueLength_Is_4() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", "code,1,city,,tags,bJWG,{5~,nv{")
                .formParam( "lat/long", "0 -4064455104123757724.2")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.tags.Items.Contains.Value.Length=4
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAddressValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "address", "code,1,city,,krihnhsp,726,sivcmmot,true")
                .formParam( "lat/long", "0 -4497774705939759626.5")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.address.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0 -4125302959081468980.9")
                .formParam( "map-digest", (String) null)
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0 -4361063228189246763.3")
                .formParam( "map-digest", "true")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestValueLength_Is_15() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0 -3753915552909338168.8")
                .formParam( "map-digest", "Q+YcBxk9aW7s4AwicOad")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Value.Length=15
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesMapDigestValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0 -4344728691109883651.9")
                .formParam( "map-digest", "9wYDPLaMUnxEmYhm9o49NnQ=")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.map-digest.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postUrlencoded_unexploded_BodyApplicationXWwwFormUrlencodedValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "lat/long", "0 -4432779115066126112.0")
                .formParam( "ykrbiuzgrwrvbs", "182.6")
            .when()
                .request( "POST", "/urlencoded_unexploded")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/urlencoded_unexploded", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/urlencoded_unexploded", response.statusCode(), responseHeaders( response));
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
