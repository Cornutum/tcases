package org.cornutum.examples;


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

public class OpenAPIRequestTestCases_PostsTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void getPosts_IdsDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "0")
            .when()
                .request( "GET", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsSize_Is_4() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "100|35|31|12")
            .when()
                .request( "GET", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", (String) null)
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "-6")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsSize_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Size=0
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsSize_Is_5() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "0|80|63|32|91")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Size=5
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsContainsValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "-1")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Contains.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsContainsValue_Is_101() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "101")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Contains.Value.Is=101
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsUnique_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "0|33|92|92")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Unique=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "1001,7700")
                .header( "X-User-Id", "0")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_2345() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "2345,7700")
                .header( "X-User-Id", "89629286")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_7700() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "7700,2345")
                .header( "X-User-Id", "0")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-User-Id", "776936916")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "")
                .header( "X-User-Id", "692210873")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "296")
                .header( "X-User-Id", "156012952")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsSize_Is_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "1001")
                .header( "X-User-Id", "260738343")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Items.Size=1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsSize_Is_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "1001,2345,7700")
                .header( "X-User-Id", "799098451")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Items.Size=3
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", ",7700")
                .header( "X-User-Id", "1029955401")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "true,1001")
                .header( "X-User-Id", "192318611")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "-747040742,7700")
                .header( "X-User-Id", "435090145")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Items.Contains.Value.Is=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsUnique_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "1001,1001")
                .header( "X-User-Id", "472407758")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Items.Unique=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XUserIdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "1001,7700")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-User-Id.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XUserIdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "1001,7700")
                .header( "X-User-Id", "")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-User-Id.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XUserIdType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "1001,2345")
                .header( "X-User-Id", "?\\%XJN9")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-User-Id.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XUserIdValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "1001,2345")
                .header( "X-User-Id", "-1")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-User-Id.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_ApprovedDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "true")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\",\"email\":\"p@i.net\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_ApprovedValue_Is_False() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
            .when()
                .request( "POST", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_32() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "true")
                .contentType( "application/json")
                .request().body( "{\"text\":\"ZwGvOPrjzu61{$ek@KVCB]|8!@Q$Wi&;xf,:Wit)rYJz,PQd{e^R5%<Vz{4e$w*M\",\"email\":\"kZ@c.sB.Bi.Ie.I9.7e.Xg.jP.Dh.net\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_ApprovedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
            .when()
                .request( "POST", "/posts")
            .then()
                // approved.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_ApprovedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", null)
            .when()
                .request( "POST", "/posts")
            .then()
                // approved.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_ApprovedType_Is_NotBoolean() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "441.0")
            .when()
                .request( "POST", "/posts")
            .then()
                // approved.Type=Not boolean
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyMediaType_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "text/plain")
                .request().body( "{}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.Media-Type=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "null")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "-558")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.email.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\",\"email\":null}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.email.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\",\"email\":471.7}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.email.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_6() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\",\"email\":\"gl.org\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.email.Value.Length=6
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_33() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\",\"email\":\"?.Bb.96.3F.sY.rQ.LF.E;.nR@o.a.edu\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.email.Value.Length=33
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"email\":\"g@Q.edu\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.text.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":null,\"email\":\"w@w.org\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.text.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":{\"mjcx\":true,\"fwzwenjgliwvrgnr\":\"a5\",\"e\":\"XRRy\"},\"email\":\"9@7.net\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.text.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextValueLength_Is_65() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\")f_fpwYXqdu21?A] 5t8uZe7$5NG0Wed bLS/6;wn_jw\\\"o5{L\\\\\\\\b*4qa eva$xZI$\",\"email\":\"Y@g.net\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.text.Value.Length=65
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\",\"email\":\"X@d.com\",\"akgyspeyu\":{},\"lnwsoivn\":true}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "I")
                .cookie( "region", "W")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"0@p.net\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_16() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", ":IEt|~VjQA_R}TA(")
                .cookie( "region", "#:iPPHOumVFtB^@r")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"JFQM3N>B1#lB/U{OHId-;1^nGH1l+ah#,#![^s\\\\brAMrXLB\\\\ u$[V><]\\\"dv*RKO;\",\"email\":\"GJ@W.kds.PHn.7DP.5GF.aNX.OZp.edu\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"R@L.net\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", null)
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"B@N.org\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "true")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"r@i.net\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "region", "h")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"*@3.net\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.country.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", null)
                .cookie( "region", "k")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"l@u.gov\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.country.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "GF:-Al}%h'$N)k5Nn")
                .cookie( "region", "#")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"=@O.edu\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.country.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "b")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"{@V.gov\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.region.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "[")
                .cookie( "region", null)
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"v@S.com\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.region.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "D")
                .cookie( "region", "+WnNcm{fkr^-5Nd.)")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"F@B.org\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.region.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "Y")
                .cookie( "region", "C")
                .cookie( "gbkjl", "-66.8")
                .cookie( "hb", "T|_")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"k@y.net\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "U")
                .cookie( "region", "c")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyMediaType_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "s")
                .cookie( "region", "4")
                .contentType( "text/xml")
                .request().body( "-771")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.Media-Type=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "T")
                .cookie( "region", "/")
                .contentType( "text/plain")
                .request().body( "")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "[")
                .cookie( "region", "6")
                .contentType( "text/plain")
                .request().body( "274.9")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "I")
                .cookie( "region", "~")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.email.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "t")
                .cookie( "region", "h")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":null}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.email.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_6() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "%")
                .cookie( "region", "%")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"AE.org\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.email.Value.Length=6
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_33() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "[")
                .cookie( "region", "f")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"`T.c5.#V.5P.-0.Su.sI.'b@r.3.b.edu\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.email.Value.Length=33
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "X")
                .cookie( "region", "-")
                .contentType( "text/plain")
                .request().body( "{\"email\":\"b@9.gov\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.text.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "A")
                .cookie( "region", "(")
                .contentType( "text/plain")
                .request().body( "{\"text\":null,\"email\":\"4@T.org\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.text.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextValueLength_Is_65() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "J")
                .cookie( "region", "t")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"e)c#9}P`CkQ <[8liE&[lM(raFH&GF3.<R,0j%YD/W`fDyS|`Sg1'dBa1N#Rf,}!e\",\"email\":\"#@W.com\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.text.Value.Length=65
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "P")
                .cookie( "region", "4")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"4@B.edu\",\"qlkhuwarohjcmmd\":{}}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "A")
            .when()
                .request( "TRACE", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "B|C")
            .when()
                .request( "TRACE", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsContainsValue_Is_C() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "C")
            .when()
                .request( "TRACE", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", null)
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "qmwcxwccseckwu|-313.6|xpecqgrrunmz|-994.4|jn|true")
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "")
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Items.Size=0
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "A|C|B")
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Items.Size=3
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "|C")
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsContainsValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "+/[+8UiJY(^XUsldUFED6*6>Z1V^U]fpMxg~w=>*o&GO%.4DSNgPY8dlaQ0h-gK]4[/o+xQ0sC=lxGnL+gCz|C")
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Items.Contains.Value=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsUnique_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "A|A")
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Items.Unique=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
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
package org.cornutum.examples;


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

public class OpenAPIRequestTestCases_PostsUseridAttributesTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void deletePostsUserIdAttributes_UserIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=true,likes=0")
                .pathParam( "userId", "0")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=false")
                .pathParam( "userId", "1012046720")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesApprovedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "likes=436312603")
                .pathParam( "userId", "0")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=true")
                .pathParam( "userId", "")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // userId.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=true")
                .pathParam( "userId", "")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // userId.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=true")
                .pathParam( "userId", "-1")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // userId.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "")
                .pathParam( "userId", "503211")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "171")
                .pathParam( "userId", "764992784")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesApprovedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=")
                .pathParam( "userId", "1025718526")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Properties.approved.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesApprovedType_Is_NotBoolean() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=-31")
                .pathParam( "userId", "610212026")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Properties.approved.Type=Not boolean
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesLikesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=true,likes=")
                .pathParam( "userId", "42562579")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Properties.likes.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesLikesType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=true,likes=Nb<l")
                .pathParam( "userId", "64646460")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Properties.likes.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesLikesValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=true,likes=-1")
                .pathParam( "userId", "546132393")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Properties.likes.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=true,vvasyxuquvohc=?5^,1@,Vs,f=Q+iXAv6")
                .pathParam( "userId", "297471232")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
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
package org.cornutum.examples;


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

public class OpenAPIRequestTestCases_PostsAttributesTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void tracePostsAttributes_AttributesDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=true;subject=A Day In Hell;likes=0")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedValue_Is_False() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=false;likes=1033374122")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_WhatMeWorry() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=true;subject=What? Me, worry?;likes=0")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", "")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", "")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";attributes=670")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";likes=240095113")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.approved.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved;likes=588204475")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.approved.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedType_Is_NotBoolean() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=0;likes=655969793")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.approved.Type=Not boolean
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=false")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.likes.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=false;likes")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.likes.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=false;likes=noxodj,]G8te^,q,/ZGRSe'")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.likes.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=false;likes=-1")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.likes.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=false;subject;likes=275507395")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.subject.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=false;subject=T6n:2 TGq64S?G55LA}?\"nCf4bt)Jda:O-1JP/r\\#*;DZkLr}5v=:[OgEJkwNsAWd#w{=acXgFE}K+}U5Hk5KT'maYikYsS{R$VAC2mS{:D/I~Sd\"%HcE.3j|/& #e2Ak5-H52vY6QA-FsF|~nyK9bCaxM.}DpZ%9,j;likes=179317882")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.subject.Value=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=false;likes=188710057;sd;ldwrnjti=701.0")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
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
package org.cornutum.examples;


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

public class OpenAPIRequestTestCases_PostTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void headPost_UserAttributesDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "0,2")
                .queryParam( "user attributes", "user type,Typical User")
            .when()
                .request( "HEAD", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeValue_Is_VIP() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "1,2")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsValue_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "2,0")
                .queryParam( "user attributes", "user type,Typical User")
            .when()
                .request( "HEAD", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "0,1")
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "0,1")
                .queryParam( "user attributes", (String) null)
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "0,1")
                .queryParam( "user attributes", "hoL$Q\\|1,S)\\>_\"")
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "0,1")
                .queryParam( "user attributes", "")
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Value.Properties.user-type.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "0,2")
                .queryParam( "user attributes", "user type,")
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Value.Properties.user-type.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "0,1")
                .queryParam( "user attributes", "user type,.0nz],>z4~H\")j\\}|`?+hI|yQc7wA^<wCP3 .+\"WdLSN1ibHJ53rT4:{+,ALZ0.3Jq+__d]c-Gsrr{R+W:Qm$\\xIaFv!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Value.Properties.user-type.Value=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "0,2")
                .queryParam( "user attributes", "user type,VIP!,vpqvpj,true,uc,true")
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", (String) null)
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", (String) null)
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "558")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsSize_Is_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "0")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Items.Size=1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsSize_Is_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "0,1,2")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Items.Size=3
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", ",1")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "t,-900.0,1")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "-636538791,1")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Items.Contains.Value.Is=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsUnique_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "0,0")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Items.Unique=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "0,1")
                .queryParam( "post?[cpfcbizklmwzg]", ",iodK,B")
                .queryParam( "post?[xshghvcozd]", "aI3I")
                .queryParam( "post?[ordgycqn]", "-937")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "{X}")
            .when()
                .request( "PATCH", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "<Y> {X} #Z")
            .when()
                .request( "PATCH", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsContainsValue_Is_Z() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "#Z")
            .when()
                .request( "PATCH", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsUnique_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "{X} #Z #Z")
            .when()
                .request( "PATCH", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", (String) null)
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "399.6")
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "")
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Items.Size=0
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_4() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "<Y> <Y> {X} <Y>")
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Items.Size=4
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "")
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsContainsValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "TVs[poLN3ceQfzfJt!8#UDTs!,y!M[|2sUSJ6y+ga`{M4BB=w$C}u}UEcFd[c(7Z|s~>vX<+:(5!!@P_pr(j_")
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Items.Contains.Value=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "0")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "true")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "13370034749586532.7")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "(?)")
            .when()
                .request( "PUT", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_MeYou() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "0")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "true")
                .formParam( "reviewer", "Me+You")
            .when()
                .request( "PUT", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // postId.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", (String) null)
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // postId.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdType_Is_NotNumber() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "pdhlklrbyc,-824.9,ywbozc,BADf($e")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // postId.Type=Not number
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "-1")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // postId.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "616970863794345540.5")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyMediaType_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "449152389518419911.4")
                .contentType( "text/plain")
                .request().body( "a")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.Media-Type=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "856261299130404139.2")
                .contentType( "application/x-www-form-urlencoded")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "89506468941322092.7")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "boolean", "true")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "211319801489572958.2")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.approved.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "229138368926041577.2")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", (String) null)
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_NotBoolean() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "679849226446415268.9")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "-717")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=Not boolean
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "894963656954781853.6")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "376648681248997619.8")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", (String) null)
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "106342093256701848.1")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "8(*A!^tM`}~\\^L<J]8K{g8JzTR3n3UtV|3+S}_9}W}WLZeht*]G_<b $K6h0j]-{vz0c([_F@'\"4E%yI=i)s.g=LFn%SSq.4M?.g1-W >AuVibu1b4au-fF<6rX)7MIH:y")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Value=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "865569324071895564.2")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "Larry Moe")
                .formParam( "fkqzxaihekd", "-")
                .formParam( "nngbtbdnmsddnlw", "-436.9")
                .formParam( "wmoowmjsaef", "-929")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
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
package org.cornutum.examples;


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

public class OpenAPIRequestTestCases_PostUseridApprovedTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void deletePostUserIdApproved_UserIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".0")
                .pathParam( "userId", ".0")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_1000() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".1")
                .pathParam( "userId", ".1000")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".0")
                .pathParam( "userId", "")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // userId.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".0")
                .pathParam( "userId", "")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // userId.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".0")
                .pathParam( "userId", ".-72.0")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // userId.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".0")
                .pathParam( "userId", ".-1")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // userId.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_1001() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".0")
                .pathParam( "userId", ".1001")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // userId.Value.Is=1001
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_ApprovedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", "")
                .pathParam( "userId", ".0")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // approved.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_ApprovedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", "")
                .pathParam( "userId", ".0")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // approved.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_ApprovedType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".-782.0")
                .pathParam( "userId", ".0")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // approved.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_ApprovedValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".557495789")
                .pathParam( "userId", ".0")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // approved.Value.Is=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
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
package org.cornutum.examples;


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

public class OpenAPIRequestTestCases_UsersTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void getUsers() {
        Response response =
            given()
                .baseUri( forTestServer())
            .when()
                .request( "GET", "/users")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/users", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/users", response.statusCode(), responseHeaders( response));
    }

    private static Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
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
