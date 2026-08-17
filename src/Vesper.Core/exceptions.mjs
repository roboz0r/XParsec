// Generated from exceptions.js.fs
export class SystemException extends Error {
  constructor(message) {
    super(message);
    this.message = message;
  }
}
export class InvalidOperationException extends SystemException {
  constructor(message) {
    super(message);
    this.message = message;
  }
}
export class ArgumentException extends SystemException {
  constructor(message) {
    super(message);
    this.message = message;
  }
}
export class ArgumentNullException extends ArgumentException {
  constructor(message) {
    super(message);
    this.message = message;
  }
}
export class NotSupportedException extends SystemException {
  constructor(message) {
    super(message);
    this.message = message;
  }
}
export class IndexOutOfRangeException extends SystemException {
  constructor(message) {
    super(message);
    this.message = message;
  }
}
export class FormatException extends SystemException {
  constructor(message) {
    super(message);
    this.message = message;
  }
}
