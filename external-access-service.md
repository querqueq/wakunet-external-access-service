## POST /externals


- This endpoint is sensitive to the value of the **UserId** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"email":"john@example.org","accessibleContent":{"contentId":5,"contentType":"post"},"expires":"2015-12-08T19:00:00Z","level":"Write"}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"value":"Lorem Ipsum"}
```

## GET /externals/:contentType/:contentId

#### Captures:

- *contentType*: (string) super type of rated content
- *contentId*: (long) id if rated content


- This endpoint is sensitive to the value of the **UserId** HTTP header.

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[{"email":"john@example.org","accessibleContent":{"contentId":1,"contentType":"post"},"uuid":"1c012ed3-877d-44b8-87ac-eae9b1ef7b1b","accessRevoked":false,"created":"2015-12-08T19:00:00Z","expires":null,"alias":"John","level":"Read","creatorId":1}]
```

## DELETE /externals/:externalid

#### Captures:

- *externalid*: (uuid) uuid v4 of external access


- This endpoint is sensitive to the value of the **UserId** HTTP header.

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## GET /externals/:externalid

#### Captures:

- *externalid*: (uuid) uuid v4 of external access

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"email":"john@example.org","accessibleContent":{"contentId":1,"contentType":"post"},"uuid":"1c012ed3-877d-44b8-87ac-eae9b1ef7b1b","accessRevoked":false,"created":"2015-12-08T19:00:00Z","expires":null,"alias":"John","level":"Read","creatorId":1}
```

## POST /externals/:externalid

#### Captures:

- *externalid*: (uuid) uuid v4 of external access


- This endpoint is sensitive to the value of the **UserId** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"email":"john@example.org","accessibleContent":{"contentId":5,"contentType":"post"},"expires":"2015-12-08T19:00:00Z","level":"Write"}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"email":"john@example.org","accessibleContent":{"contentId":1,"contentType":"post"},"uuid":"1c012ed3-877d-44b8-87ac-eae9b1ef7b1b","accessRevoked":false,"created":"2015-12-08T19:00:00Z","expires":null,"alias":"John","level":"Read","creatorId":1}
```

## POST /externals/:externalid/alias

#### Captures:

- *externalid*: (uuid) uuid v4 of external access

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"value":"Lorem Ipsum"}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## GET /externals/:externalid/content

#### Captures:

- *externalid*: (uuid) uuid v4 of external access

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"profiles":[{"email":null,"profilePicture":null,"state":null,"employedSince":null,"userId":1,"picture":null,"gender":null,"about":null,"firstName":"John","surname":"Doe"}],"externalAccess":{"email":"john@example.org","accessibleContent":{"contentId":1,"contentType":"post"},"uuid":"1c012ed3-877d-44b8-87ac-eae9b1ef7b1b","accessRevoked":false,"created":"2015-12-08T19:00:00Z","expires":null,"alias":"John","level":"Read","creatorId":1},"content":{"subPostCount":2,"text":"Hi there!","created":"2015-12-08T19:00:00Z","subPosts":[{"subPostCount":0,"text":"Welcome to this group.","created":"2015-12-08T19:02:00Z","subPosts":[],"groupId":1,"id":2,"updated":null,"type":"fullPost","contentKey":{"contentId":0,"contentType":"post"},"creatorId":2,"parentPostId":1},{"subPostCount":0,"text":"Thanks!","created":"2015-12-08T19:03:52Z","subPosts":[],"groupId":1,"id":3,"updated":null,"type":"fullPost","contentKey":{"contentId":0,"contentType":"post"},"creatorId":1,"parentPostId":1}],"groupId":1,"id":1,"updated":null,"type":"fullPost","contentKey":{"contentId":1,"contentType":"post"},"creatorId":1,"parentPostId":null}}
```

## POST /externals/:externalid/content

#### Captures:

- *externalid*: (uuid) uuid v4 of external access

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"subPostCount":2,"text":"Hi there!","created":"2015-12-08T19:00:00Z","subPosts":[{"subPostCount":0,"text":"Welcome to this group.","created":"2015-12-08T19:02:00Z","subPosts":[],"groupId":1,"id":2,"updated":null,"type":"fullPost","contentKey":{"contentId":0,"contentType":"post"},"creatorId":2,"parentPostId":1},{"subPostCount":0,"text":"Thanks!","created":"2015-12-08T19:03:52Z","subPosts":[],"groupId":1,"id":3,"updated":null,"type":"fullPost","contentKey":{"contentId":0,"contentType":"post"},"creatorId":1,"parentPostId":1}],"groupId":1,"id":1,"updated":null,"type":"fullPost","contentKey":{"contentId":1,"contentType":"post"},"creatorId":1,"parentPostId":null}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## POST /externals/:externalid/notify

#### Captures:

- *externalid*: (uuid) uuid v4 of external access


- This endpoint is sensitive to the value of the **UserId** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"url":"http://www.wakunet.at/externals/a91a964b-6ace-454b-a5ac-5a37315d23f3"}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

